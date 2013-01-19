{-# LANGUAGE RebindableSyntax, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances,
    OverlappingInstances, TypeFamilies, ScopedTypeVariables #-}

module Generator.Prelude 
  ( ProMonad(..)
  , Num(..)
  , Int
  , Ptr(NullPtr)
  , Show(..)
  -- * Monad extensions
  , (>>)
  , (>=>)
  , when
  -- * Compare
  , ExpEq
  , (==)
  , (/=)
  -- * Misc
  , const
  , (.)
  , ($)
  , putStrLn
  , P.id
  -- * Bool operators
  , Bool(..)
  , not
  , (&&)
  , (||)
  , ifThenElse
  , true
  , false
  -- * List operations
  , or, and    -- (Elem xs ~ Bool, Iterable xs) => Exp w xs -> Exp w Bool
  , any, all   -- (Iterable xs, Typeable (Elem xs), P.Eq (Elem xs)) =>
               --   (Exp w (Elem xs) -> Exp w Bool) -> Exp w xs -> Exp w Bool
  , maximum    -- (Iterable xs, Typeable (Elem xs), P.Ord (Elem xs)) => 
               --   Exp w xs -> Exp w (Elem xs)
  , minimum    -- ...
  , (++) -- (Iterable a, Typeable (Elem a)) => Exp w a -> Exp w a -> Exp w a
  , (+=) -- (Iterable a, Typeable (Elem a)) => Exp w a -> Exp w (Elem a) -> Exp w a
  , tail
  , (!!)
  , map
  , foldl
  , init
  , reverse
  , take
  , drop
  , length
  , null
  , head
  , last
  , traverse_
  , for
  , for_
  , sum
  , filter
  , filter'
  -- * Basic number function 
  , quot, rem  -- Exp w Int -> Exp w Int -> Exp w Int 
  , min
  , max 
  , (<)
  , (<=)
  , (>)
  , (>=)
  -- * Pairs
  , mkPair
  , fst, snd
  , P.flip
  -- * Maps
  , mapErase, mapInsert, mapGet
  -- * Iterators
  , cBegin, cEnd
  , advanceIter, derefIter
  , fromIter
  , list
  ) where

import Generator.Expr hiding (ifThenElse)
import qualified Generator.Expr as E
import Generator.Internal.Expr (ProMonad(..),FPtr(..),Magic(..))
import Generator.Interpreter 
import Generator.ClassExpr
import Generator.Types 
import Prelude (Show(..), Num(..), (.), Bool(..), Int, ($), const)
import qualified Prelude as P

(>=>) :: ProMonad m => (a -> m b) -> (Magic (m b) -> m c) -> a -> m c
(>=>) f h x = f x >>= h

cBegin :: (Iterable a, Typeable (Elem a)) => Exp w a -> Exp w (Cls (Iter a))
cBegin (iterable :: Exp w a) = method0 (beginIter (P.undefined :: a))
                                       (useList iterable)
                                       (read contents >=> \ls -> create (Iter (ls, 0)))

cEnd :: (Iterable a, Typeable (Elem a)) => Exp w a -> Exp w (Cls (Iter a))
cEnd (iterable :: Exp w a) = method0 (endIter (P.undefined :: a))
                                     (useList iterable)
                                     (read contents >=> \ls -> create (Iter (ls, P.length ls)))

fromIter :: (Iterable a, Typeable (Elem a)) => Exp w (Cls a) -> Exp w [Elem a] 
fromIter = useIter . initClass1 contents . useIter


list :: Typeable a => [Exp w a] -> Exp w [a]
list exps = initClass0 [] >>= \newLs -> P.foldl (+=) (useIter newLs) exps
--list = List

infixr 5 ++, +=
(++) :: (Iterable a, Typeable (Elem a)) => Exp w a -> Exp w a -> Exp w a
xs' ++ ys' = useList xs' >>= \xs ->
             useList ys' >>= \ys -> insert xs ys >> useIter xs
  where insert :: (Iterable a, Typeable (Elem a)) => Exp w (Cls a) -> Exp w (Cls a) -> Exp w ()
        insert xs ys = method3 "insert" xs (\as _ yIt _ -> read (\(Iter p) -> P.fst p) yIt
                                        >>= \bs -> write (buildIter . (P.++ bs) . contents) as)
                               (cEnd $ useIter xs) (cBegin $ useIter ys) (cEnd $ useIter ys)

(+=) :: (Iterable a, Typeable (Elem a)) => Exp w a -> Exp w (Elem a) -> Exp w a
xs' += x' = useList xs' >>= \xs ->
            x'  >>= \x  -> push xs x >> useIter xs
  where push :: (Iterable a, Typeable (Elem a))
             => Exp w (Cls a) -> Exp w (Elem a) -> Exp w ()
        push xs x = method1 "push_back" xs
            (\as a -> write (buildIter . (P.++ [a]) . contents) as) x

putStrLn :: Exp w P.String -> Exp w ()
putStrLn = function1 "print" (const ())

tail,init,reverse :: (Iterable xs, Typeable (Elem xs)) => Exp w xs -> Exp w xs
tail = mkFunction1 "tail" $ \xs ->
            assert (not $ null xs) (drop 1 xs)
                (value $ Str "Generator.Prelud.tail: empty list")
init    = E.eInit
reverse = E.eReverse

take,drop :: (Iterable xs, Typeable (Elem xs)) => Exp w Int -> Exp w xs -> Exp w xs
take = E.eTake
drop = E.eDrop

assert :: Typeable a => Exp w Bool -> Exp w a -> Exp w Str -> Exp w a
assert b a s = if b then a else throw s >> a
  where throw :: Exp w Str -> Exp w ()
        throw = function1 "throw" (\(Str s) -> P.error s)

head,last :: (Iterable xs, Typeable (Elem xs)) => Exp w xs -> Exp w (Elem xs)
head = mkFunction1 "tail" $ \xs ->
            assert (not $ null xs) 
                 (method0 "front" (useList xs) (read (P.head . contents))) $
                    value $ Str "Generator.Prelude.head: empty list"
last = mkFunction1 "last" $ \xs ->
            assert (not (null xs)) 
                 (method0 "back"  (useList xs) (read (P.last . contents))) $
                    value $ Str "Generator.Prelude.last: empty list"


ifThenElse :: Typeable a => Exp w Bool -> Exp w a -> Exp w a -> Exp w a
ifThenElse = E.ifThenElse --p (const tru) (const fls) (value ())

infixl 1 >>

--(>>) :: ProMonad a -> ProMonad b -> ProMonad b
f >> g = f >>= \_ -> g

instance Show a => Show (Exp w a) where
    show = show . run

instance (Show a, Typeable a, Num a, P.Ord a) => Num (Exp w a) where
    fromInteger = value . fromInteger
    (+) = binary "+" (+)
    (-) = binary "-" (-)
    (*) = binary "*" (*)
    abs    = function1 "abs" abs
    signum = mkFunction1 "sgn" $ \x -> if x > 0 then return 1 else (if x < 0 then return (-1) else return 0)

infixl 7 `quot`, `rem`
quot, rem :: Exp w Int -> Exp w Int -> Exp w Int
quot = binary "/" P.quot
rem = binary "%" P.rem

infix 4 <
infix 4 >
infix 4 <=
infix 4 >=
(<),(>),(<=),(>=) :: (Typeable a, P.Ord a) => Exp w a -> Exp w a -> Exp w Bool
(<) = binary "<" (P.<)
(>) = binary ">" (P.>)
(<=) = binary "<=" (P.<=)
(>=) = binary ">=" (P.>=)

min, max :: (Typeable a, P.Ord a) => Exp w a -> Exp w a -> Exp w a
min a b = ifThenElse (a <= b) a b 
max a b = ifThenElse (a >= b) a b 

infixl 3 &&
infixl 3 ||
(&&) :: Exp w Bool -> Exp w Bool -> Exp w Bool
(&&) = binary "&&" (P.&&)

(||) :: Exp w Bool -> Exp w Bool -> Exp w Bool
(||) = binary "||" (P.||)

map :: (Typeable a, Typeable b) => (Exp w a -> Exp w b) -> Exp w [a] -> Exp w [b]
map = eMap

--foldl :: (Typeable a, Typeable (Elem xs), Iterable xs) => (Exp w a -> Exp w (Elem xs) -> Exp w a) -> Exp w a -> Exp w xs -> Exp w a
--foldl = eFoldL

foldl :: (Typeable a, Typeable (Elem xs), Iterable xs, P.Eq (Elem xs))
      => (Exp w a -> Exp w (Elem xs) -> Exp w a) -> Exp w a -> Exp w xs -> Exp w a
foldl f i' xs' = do
    i <- i'
    xs <- xs'
    begin <- cBegin xs
    end   <- cEnd   xs
    while (const $ begin /= end) (\a -> do
                                      elem <- f a (derefIter begin)
                                      advanceIter begin 1
                                      elem
                                 ) i
-- cBegin :: (Iterable a, Typeable (Elem a)) => Exp w a -> Exp w (Cls (Iter a))
--while :: Typeable a => (Exp w a -> Exp w Bool) -> (Exp w a -> Exp w a) -> Exp w a -> Exp w a


or, and :: (Iterable xs, Bool ~ (Elem xs)) => Exp w xs -> Exp w Bool
or  = any P.id
and = all P.id

any, all :: (Iterable xs, Typeable (Elem xs), P.Eq (Elem xs))
         => (Exp w (Elem xs) -> Exp w Bool) -> Exp w xs -> Exp w Bool
any f = foldl (\a -> (a ||) . f) false 
all f = foldl (\a -> (a &&) . f) true


length :: (Iterable xs, Typeable (Elem xs)) => Exp w xs -> Exp w Int
length xs = method0 "size" (useList xs) (read (P.length . contents))

null :: (Iterable xs, Typeable (Elem xs)) => Exp w xs -> Exp w Bool
null xs = method0 "empty" (useList xs) (read (P.null . contents))

(!!) :: (Iterable xs, Typeable (Elem xs)) => Exp w xs -> Exp w Int -> Exp w (Elem xs)
xs !! n = head . drop n $ xs

traverse_ :: (Typeable (Elem as), Iterable as, P.Eq (Elem as)) => (Exp w (Elem as) -> Exp w b) -> Exp w as -> Exp w ()
traverse_ f as = foldl (\unit a -> f a >> unit) (0::Exp w Int {-()-}) as >> value ()-- TODO allow unit
for :: (Typeable a, Typeable b) => Exp w [a] -> (Exp w a -> Exp w b) -> Exp w [b]
for = P.flip map
for_ :: (Typeable (Elem as), Iterable as, P.Eq (Elem as)) => Exp w as -> (Exp w (Elem as) -> Exp w b) -> Exp w ()
for_ = P.flip traverse_

minimum xs = foldl min (head xs) xs
maximum xs = foldl max (head xs) xs

true  = value True
false = value False

when :: Exp w Bool -> Exp w () -> Exp w ()
when p e = ifThenElse p e (value ())

infixr 4 ==
infixr 4 /=

instance P.Eq (Exp w a) where
    (==) = P.error "== on Exp"
    (/=) = P.error "/= on Exp"

class ExpEq e a where
    (==) :: e a -> e a -> e Bool
    (/=) :: e a -> e a -> e Bool

instance (Typeable a, P.Eq a) => ExpEq (Exp w) a where
    (==) = binary "==" (P.==)
    (/=) = binary "!=" (P./=)

instance (Class c, P.Eq c) => ExpEq (Exp w) (Cls c) where
    a == b = binaryHS "==" (\c1 c2 -> do
            cls1 <- read P.id c1
            cls2 <- read P.id c2
            return $ cls1 P.== cls2
        ) a b 
    a /= b = binaryHS "!=" (\c1 c2 -> do
            cls1 <- read P.id c1
            cls2 <- read P.id c2
            return $ cls1 P./= cls2
        ) a b 

not :: Exp w Bool -> Exp w Bool
not = unary "!" P.not


instance P.Eq (Ptr t a) where
    (==) = P.error "== on Ptr"

instance P.Eq a => P.Eq (Cls a) where
    (==) = P.error "== on Cls"


filter' :: (P.Eq (Elem xs), Typeable (Elem xs), Iterable xs) => (Exp w (Elem xs) -> Exp w Bool) -> Exp w xs -> Exp w xs
filter' p xs = foldl (\a b -> if p b then a += b else a) (return $ buildIter []) xs
filter :: (P.Eq (Elem xs), Typeable (Elem xs), Iterable xs) => (Exp w (Elem xs) -> Exp w Bool) -> Exp w xs -> Exp w xs
filter p xs = filter' (not . p . deConst) xs 
  where 
    filter' :: (Iterable xs, Typeable (Elem xs)) => 
               (Exp w (Const (Elem xs)) -> Exp w Bool) -> Exp w xs -> Exp w xs
    filter' p xs = function2 "filter" 
                             (\xs f -> buildIter $ P.filter (P.not . unFun f . Const) (contents xs)) 
                             xs 
                             (functionPtr1 p)

fst :: (Typeable a, Typeable b) => Exp w (Cls (a, b)) -> Exp w a
fst = functionHS1 "get21" (read P.fst)
snd :: (Typeable a,Typeable b) => Exp w (Cls (a, b)) -> Exp w b
snd = functionHS1 "get22" (read P.snd)

sum :: (Iterable a, Elem a ~ Int) => Exp w a -> Exp w Int -- Exp w [Int] -> Exp w Int
sum = foldl (+) 0

mkPair :: (Typeable a, Typeable b) => Exp w a -> Exp w b -> Exp w (Cls (a, b))
mkPair = initClass2 (\ap bp -> (ap,bp))

mapErase :: (Typeable key, Typeable value) => Exp w (Map key value)
                                     -> Exp w (Const key) -> Exp w ()
mapErase cls = method1 "erase" (useList cls) (P.error "mapErase")
mapInsert :: (Typeable key, a :<<: value , Typeable a)
          => Exp w (Map key value)
          -> Exp w (Cls (Const key, value)) -> Exp w ()
mapInsert cls = method1 "insert" (useList cls) (P.error "mapInsert")
mapGet :: (Typeable key, Typeable value) => Exp w (Map key value)
       -> Exp w (Const key) -> Exp w value
mapGet cls = method1 "operator[]" (useList cls) (P.error "mapGet")

advanceIter :: (Iterable a) => Exp w (Cls (Iter a)) -> Exp w Int -> Exp w ()
advanceIter = functionHS2 "std::advance" $ \cls m -> do
    write (\(Iter (xs, i)) -> Iter (xs, P.min (P.length xs) (i P.+ m))) cls
    return ()

derefIter :: (Iterable a, Typeable (Elem a)) => Exp w (Cls (Iter a)) -> Exp w (Elem a)
derefIter = unsafeDeref . pt
  where
    pt :: (Typeable (Elem a), Iterable a) => Exp w (Cls (Iter a)) -> Exp w (InternalPtr (Elem a))
    pt = expMap (read (\(Iter (xs, i)) -> xs P.!! i) >=> \elem -> createPtr elem)
