{-# LANGUAGE GADTs, ExistentialQuantification, ViewPatterns, ScopedTypeVariables,
   TypeFamilies, FlexibleInstances #-}
module Generator.Interpreter
  ( run -- Exp w c -> c
  ) where
  
import Generator.Internal.Expr as IE hiding (return)
import Generator.Expr
import Generator.ClassExpr
import Generator.Internal.Types
import Generator.Types
import Generator.Interpreter.State

import Data.Tuple (swap)
import Data.List (mapAccumL)
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Unsafe.Coerce
import System.IO.Unsafe 
import Data.IORef

-- | Run an expression with the default state
run :: Exp w c -> c
run  = fst . run' defaultState

-- | Run a HS computation 
runHS' :: State w -> HS w a -> (a, State w)
runHS' s hs = case hs of
    Create m -> let key  = next s
                    s'   = (s { next    = key + 1
                              , classes = I.insert key (Opaque m) (classes s)})
                  in (Ptr key,s')
    Then m f -> let (a, s') = runHS' s m
                    in runHS' s' (f a)
    Do a    -> (a, s)
    DoExp a -> run' s a
    Read f (Ptr key) ->
        case I.lookup key (classes s) of
            Nothing -> error "key not present (Interpreter: UnsafeRead)"
            Just (Opaque v) -> (f (unsafeCoerce v), s)
    Write f (Ptr key) -> 
        case I.lookup key (classes s) of
            Nothing -> error "key not present (Interpreter: UnsafeWrite)"
            Just (Opaque v) -> 
              ((), s { classes = I.insert key (Opaque $ f (unsafeCoerce v)) (classes s) 
                     })
    Get   -> (world s, s)
    Put w -> ((), s { world = w } )


run' :: State w -> Exp w a -> (a,State w)
run' s exp = case exp of
    Call _ f v -> case f of
        FTH  f -> let (x,s') = tupleRun run' s v in runHS' s' (f x)
        PureH f -> runHS' s (f v)
    WrapFunction f _ inp -> run' s (f inp)
    FunctionPtr f -> (FPtr $ \x -> fst $ run' s (f $ back x),s ) 
    Prim  x   -> (x, s)
    Return a  -> (a, s) 
    Let m f  -> let (r,s') = run' s m
                  in run' s' (f (Return r))
    While cond body init -> while' (run' s . cond $ init) cond body init
      where
        while' :: Typeable b => (Bool, State w) -> (Exp w b -> Exp w Bool) -> (Exp w b -> Exp w b) -> Exp w b -> (b, State w)
        while' (False, s') _ _ st = run' s' st
        while' (True , s') c b st = while' (run' s'' (c st')) c b st'
          where (nakedSt, s'') = run' s' (b st)
                st' = value nakedSt
    If p a b -> let (p', s') = run' s p
        in case p' of
            True  -> run' s' a
            False -> run' s' b
    ForAll ds f  -> let (ds', s') = run' s ds
                     in swap $ mapAccumL (\st elem -> swap $ run' st (f $ Prim elem)) s' ds'   
    ManipulateList Reverse xs -> let (contents -> xs', s') = run' s xs
                                  in (buildIter $ reverse xs', s')
    ManipulateList (AdvanceBegin        n) xs -> let (contents -> xs', s') = run' s xs
                                                     (n', s'') = run' s' n
                                                  in (buildIter $ drop n' xs', s'')
    ManipulateList (DeclineEnd          n) xs -> let (contents -> xs', s') = run' s xs
                                                     (n', s'') = run' s' n
                                                  in (buildIter $ take (length xs' - n') xs', s'')
    ManipulateList (AdvanceEndFromBegin n) xs -> let (contents -> xs', s') = run' s xs
                                                     (n', s'') = run' s' n
                                                  in (buildIter $ take n' xs', s'')
    ManipulateList (DeclineBeginFromEnd n) xs -> let (contents -> xs', s') = run' s xs
                                                     (n', s'') = run' s' n
                                                  in (buildIter $ drop (length xs' - n') xs', s'')
    UseList xs -> run' s $ initClass1 id xs 
    UseIterable cls -> runGetCls s cls
    FMap  f x -> let (x', s') = run' s x in runHS' s' (f x')
    IterMap f xs -> let (xs', s') = run' s xs
                        (xs'',s'') = foldl (\(bs,st) a -> let (b,st') = runHS' st (f a) in (bs ++ [b], st')) ([],s') (contents xs')
                     in (buildIter xs'', s'')
    x -> error $ "run': " ++ inspect x

runGetCls :: Class c => State w -> Exp w (Cls c) -> (c, State w)
runGetCls s exp = case run' s exp of
    (Cls (Ptr key), s') ->
        case I.lookup key (classes s') of
        Nothing         -> error "key not present"
        Just (Opaque v) -> (unsafeCoerce v,s)

