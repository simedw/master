{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck

import Generator.Expr
import Generator.Internal.Types
import Generator.Types
import qualified Generator.Prelude as G
import Generator.Compiler
import Generator.Interpreter
import Control.Monad.Identity
import qualified Test.QuickCheck.Monadic as QC
import Test.QuickCheck.Monadic hiding (run)

class CRead a where
    cread :: String -> a

instance CRead Int where
    cread = read

instance CRead Bool where
    cread s = case (read s :: Int) of
        1 -> True
        0 -> False
        _ -> error "could not cread Bool"

class CShow a where
    cshow :: a -> String

instance CShow Int where
    cshow = show

instance CShow Bool where
    cshow True  = show 1
    cshow False = show 0

printValue :: Typeable a => Exp w a -> Exp w ()
printValue = function1 "print" (const ())

tolist :: Typeable a => [a] -> Exp w [a]
tolist xs = G.list $ map G.return xs

interpreter :: CShow a => Exp w a -> PropertyM IO String
interpreter = return . cshow . run
compiler :: (Typeable a, CShow a) => Exp w a -> PropertyM IO String
compiler    = QC.run . compileEval compileToFile . (\a -> printValue a G.>> G.return (0::Int))
optimized :: (Typeable a, CShow a) => Exp w a -> PropertyM IO String
optimized   = QC.run . compileEval compileToFileOSE . (\a -> printValue a G.>> G.return (0::Int))

compilerCfg = stdArgs { maxSuccess = 5 }
checkCompiler :: Testable prop => prop -> IO ()
checkCompiler p = quickCheckWith compilerCfg p

checkCompilerDepth d p = quickCheckWith (compilerCfg{maxSuccess = d}) p

test :: (CRead a, Eq a) => Exp w a -> a -> (Exp w a -> PropertyM IO String) -> Property
test x y f = monadicIO $ f x >>= \res -> assert $ cread res == y

runTests :: (Typeable a, CShow a, CRead a, Eq a) => Exp w a -> a -> Property
runTests a b = conjoin [t interpreter, t compiler, t optimized]
  where t = test a b

prop_add :: Int -> Int -> Property
prop_add a b = runTests (G.return a G.+ G.return b) (a + b)

prop_filter_sum :: Int -> [Int] -> Property
prop_filter_sum p xs = runTests (G.sum (G.filter (G.> G.return p) $ tolist xs)) 
                                (sum (filter (>p) xs))

prop_wrap_function1 :: Int -> Int -> Property
prop_wrap_function1 x y = runTests (wrapFunction1 (G.+ G.return x) (G.return y))
                                   ((\a -> (a + x)) y)

prop_wrap_filter :: Int -> Property
prop_wrap_filter x = runTests (
    G.return x G.>>= \randomInt ->
    G.sum $ G.filter (useMagicFunction randomInt) (G.list [1,2,3,4,5 ])
    ) (sum $ filter (useMagicFunction' x) [1,2,3,4,5])
 where 
   useMagicFunction' x y = x > y || False
   useMagicFunction x y = x G.> y G.|| G.false


prop_or :: [Bool] -> Property
prop_or bs = runTests (G.or . G.list . map G.return $ bs) (or bs)

prop_and :: [Bool] -> Property
prop_and bs = runTests (G.and . G.list . map G.return $ bs) (and bs)

prop_all :: [Int] -> Property
prop_all is = runTests (G.all magicFunction is') (all magicFunctionHS is)
  where
    is' = G.list $ map G.return is
    magicFunction   = (G.> G.sum is' `G.quot` G.length is')
    magicFunctionHS = (  >   sum is    `quot`   length is)
prop_any :: [Int] -> Property
prop_any is = runTests (G.any magicFunction is') (any magicFunctionHS is)
  where
    is' = G.list $ map G.return is
    magicFunction   = (G.> G.sum is' `G.quot` G.length is')
    magicFunctionHS = (  >   sum is    `quot`   length is)

prop_eq_list :: [Int] -> [Int] -> Property
prop_eq_list is js = runTests (is' G.== js') (is == js)
  where is' = G.list $ map G.return is
        js' = G.list $ map G.return js

prop_abs_sign :: Int -> Property
prop_abs_sign x = runTests  (G.return x G.>>= \x' -> G.abs x' G.* G.signum x' G.== x')
                            (abs x * signum x == x)

prop_head_drop :: [Int] -> Property
prop_head_drop xs = runTests (testit' xs) (testit xs)
  where
    testit [] = 0
    testit x  = head x + testit (drop 1 x)
    testit' = flatten . G.list . map G.return
    flatten xs = G.snd $ while (\x -> G.length (G.fst x) G.> G.return 0)
                       (\x  -> G.fst x G.>>=
                        \as -> G.snd x G.>>=
                        \s  -> G.mkPair (G.drop (G.return 1) as) (s G.+ G.head as))
                       (G.mkPair xs (G.return 0))

prop_fold :: [Int] -> Int -> Property
prop_fold xs x = runTests (G.foldl (G.+) (G.return x) $ G.list $ map G.return xs)
                          (foldl (+) x xs)

prop_map :: [Int] -> Int -> Property
prop_map xs i = runTests (G.sum . G.map (`G.quot` (G.return z)) $ G.list . map G.return $ xs)
                         (sum $ map (`quot` z) xs)
  where z = mkNonZero i

mkNonZero i = if i == 0 then 1 else i

prop_quot :: Int -> Int -> Property
prop_quot a b = runTests (G.return a `G.quot` G.return z) (a `quot` z)
  where z = mkNonZero b


prop_rem :: Int -> Int -> Property
prop_rem a b = runTests (G.return a `G.rem` G.return z) (a `rem` z)
  where z = mkNonZero b

prop_quot_rem :: Int -> Int -> Property
prop_quot_rem x y' = runTests ((a `G.quot` b)G.*b G.+ (a `G.rem` b) G.== a)
                              ((x   `quot` y)  *y   + (x   `rem` y)   == x)
  where a = G.return x
        b = G.return y
        y = mkNonZero y'

prop_head :: [Int] -> Property
prop_head xs = length xs > 0 ==> runTests (G.head $ G.list $ map G.return xs) (head xs)

prop_tail :: [Int] -> Property
prop_tail xs = length xs > 0 ==> runTests (G.sum $ G.tail $ G.list $ map G.return xs) (sum $ tail xs)

prop_listmanip1 :: [[Int]] -> Int -> Int -> Int -> Property
prop_listmanip1 xss n' k' i = runTests (G.sum $ G.map (fC (G.return n) (G.return k) (G.return i)) xss')
                                       (sum   $   map (fH           n            k            i ) xss )
  where xss' = G.list $ map (G.list . map G.return) xss
        mkSmaller x = x `mod` 1 + if null xss then 0 else maximum (map length xss)
        n = mkSmaller n'
        k = mkSmaller k'
        fC a b c = G.foldl (G.+) c . G.drop b . G.reverse . G.take a . G.reverse
        fH a b c =   foldl   (+) c .   drop b .   reverse .   take a .   reverse

prop_while :: Property
prop_while = do
    x <- growingElements [0..1000]
    runTests (cppFib $ G.return x) (fibs !! x)
  where fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]

cppFib :: Exp w Int -> Exp w Int
cppFib a = G.ifThenElse (a G.== G.return 0) (G.return 0) $ G.snd . G.snd $
              while ((G.> (G.return (0::Int))) . G.fst)
                    (\triple -> G.fst triple G.>>= \n ->
                                G.snd triple G.>>= \tuple ->
                                G.fst tuple  G.>>= \fibPrev ->
                                G.snd tuple  G.>>= \fibCurr ->
                                G.mkPair (n G.- G.return 1) $ G.mkPair fibCurr $ fibPrev G.+ fibCurr)
                    (G.mkPair (a G.- 1) $ G.mkPair (G.return (0::Int)) (G.return (1::Int)))

prop_duplicate_fun :: Int -> Int -> Property 
prop_duplicate_fun x y = runTests (let inc = mkFunction1 "inc" (1+)
                                in inc (G.return x) G.+ inc (G.return y))
                                (2 + x + y )

prop_null :: [Int] -> Property 
prop_null xs = runTests (G.null $ tolist xs) (null xs)

prop_mkfun3 :: Int -> [Int] -> Bool -> Property
prop_mkfun3 x ys a = runTests (apa x' ys' a') (null $ filter ((a &&) . (x >)) ys)
  where x'  = G.return x
        ys' = G.list $ map G.return ys
        a'  = G.return a

apa :: Exp w Int -> Exp w [Int] -> Exp w Bool -> Exp w Bool
apa = mkFunction3 "apanitradet" $ \ x ys pre -> G.null $ G.filter (hej x pre) ys
hej x pre i = pre G.&& x G.> i

prop_any_null :: [Bool] -> Property
prop_any_null bs = runTests (G.not (G.any id bs') G.== G.null (G.filter id bs'))
                            (  not (  any id bs )   ==   null (  filter id bs ))
  where bs' = G.list $ map G.return bs

prop_and_all :: [Bool] -> Property
prop_and_all bs = runTests (G.and (G.map id bs') G.== G.all id bs')
                           (  and (  map id bs )   ==   all id bs )
  where bs' = G.list $ map G.return bs

{-
prop_iter :: [Int] -> Property
prop_iter is = runTests (itersum is')
                        (sum is)
  where is' = G.list $ map G.return is
        itersum :: Exp w [Int] -> Exp w Int
        itersum is = do
            beg <- G.cBegin is
            end <- G.cEnd   is
            while (\_ -> beg /= end)
-}

prop_list_concat :: [Int] -> [Int] -> Int -> Property
prop_list_concat xs ys z = runTests (xs' G.++ ys' G.+= z' G.>>= \as -> G.head as + G.sum as)
                           (let as = xs    ++ ys   ++ [z]             in head as +   sum as)
  where
    xs' = tolist xs
    ys' = tolist ys
    z'  = G.return z

runAll = do
  checkCompiler prop_list_concat
  checkCompiler prop_mkfun3
  checkCompiler prop_wrap_filter
  checkCompiler prop_null
  checkCompiler prop_while
  checkCompiler prop_duplicate_fun
  checkCompiler prop_head
  checkCompiler prop_tail
  checkCompiler prop_listmanip1
  checkCompiler prop_quot_rem
  checkCompiler prop_rem
  checkCompiler prop_quot
  checkCompiler prop_fold
  checkCompiler prop_map
  checkCompiler prop_eq_list
  checkCompiler prop_or
  checkCompiler prop_and
  checkCompiler prop_all
  checkCompiler prop_any
  checkCompiler prop_add
  checkCompiler prop_abs_sign
  checkCompiler prop_filter_sum
  checkCompiler prop_wrap_function1
  checkCompiler prop_head_drop
  checkCompiler prop_any_null 
  checkCompiler prop_and_all 

main = runAll


{-
instance (Num a, Typeable a, Arbitrary a) => Arbitrary (Exp w a) where
    arbitrary = arbitrary >>= \r -> sized (arbExp $ G.return r)

arbExp :: (Num a, Typeable a, Arbitrary a) => Exp w a -> Int -> Gen (Exp w a)
arbExp x 0 = return $ x 
arbExp x n = frequency [
     (2, liftM (G.return) arbitrary)
    ,(1, liftM2 (G.+) arbitrary arbitrary)
    ,(1, liftM (G.+ x) arbitrary )
    ]
-}
