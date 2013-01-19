{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts,
    ScopedTypeVariables #-}
module Generator.Expr
  ( Exp
  , Ptr
  , value       -- Typeable a => a -> Exp w a
  -- * Model functions
  , function0   -- Typeable a => FunctionIdent -> a -> Exp w a
  , function1   -- (Typeable b, Typeable a) => FunctionIdent -> (a -> b) 
                --                                           -> Exp w a -> Exp w b
  , function2   -- ...
  , function3   -- ...
  , function4   -- ... 
  , functionHS1 -- (Typeable b, Typeable a) => FunctionIdent -> (a -> HS w b) 
                --                                           -> Exp w a -> Exp w b
  , functionHS2 -- ...
  , binary      -- (Typeable b, Typeable c, Typeable a) => 
                --  FunctionIdent -> (a -> b -> c) -> Exp w a -> Exp w b -> Exp
  , binaryHS    -- (Typeable b, Typeable c, Typeable a) => 
                --  FunctionIdent -> (a -> b -> c) -> Exp w a -> Exp w b -> Exp
  , unary       -- (Typeable b, Typeable a) => FunctionIdent -> (a -> b) -> Exp w a -> Exp w b 
  -- * Create functions 
  , wrapFunction0
  , wrapFunction1
  , wrapFunction2
  , wrapFunction3
  , mkFunction0
  , mkFunction1
  , mkFunction2
  , mkFunction3
  -- * Function Pointers
  , functionPtr1
  , functionPtr2
  -- * Control structures 
  , while       -- Typeable a => (Exp w a -> Exp w Bool) -> (Exp w a -> Exp w a)
                --                                       ->  Exp w a -> Exp w a
  , ifThenElse  -- Typeable b =>  Exp w Bool -> (Exp w a -> Exp w b)
                --                           -> (Exp w a -> Exp w b) 
                --                           ->  Exp w a -> Exp w b
  -- * List operators
  , eMap
  , eInit
  , eReverse
  , eDrop
  , eTake
  , expMap, iterMap
  -- * Types 
  , unsafeDeref
  , reference
  , mkConst
  , mkConstList
  , deConst
  , mkRef
  , rmRef
  ) where
    
import Generator.Internal.Expr
import Generator.Internal.Tuple

import Generator.Types 
import Foreign.Storable
import Data.Tuple.Curry
import Prelude hiding (return,(>>=))

value :: Typeable a => a -> Exp w a
value = Prim

mkConst :: Typeable a => Exp w a -> Exp w (Const a)
mkConst = FMap $ return . Const
mkConstList :: (Iterable a, Iterable b, Elem b ~ Const (Elem a), Typeable (Elem a))
            => Exp w a -> Exp w b
mkConstList = IterMap $ return . Const

deConst :: Typeable a => Exp w (Const a) -> Exp w a
deConst = FMap $ return . unConst

mkRef :: Typeable a => Exp w a -> Exp w (Ref a)
mkRef = FMap $ return . Ref

rmRef :: Typeable a => Exp w (Ref a) -> Exp w a
rmRef = FMap $ return . unRef

expMap :: (Typeable a, Typeable b, Basetype a ~ Basetype b)
       => (a -> HS w b) -> Exp w a -> Exp w b
expMap = FMap

iterMap :: (Iterable as, Iterable bs, Basetype (Elem as) ~ Basetype (Elem bs))
        => (Elem as -> HS w (Elem bs)) -> Exp w as -> Exp w bs
iterMap = IterMap

ifThenElse :: Typeable a => Exp w Bool -> Exp w a -> Exp w a -> Exp w a
ifThenElse = If

function0 :: Typeable a => FunctionIdent -> a -> Exp w a
function0 ident x = Call (FCall ident) (FTH $ \_ -> Do x) T0

function1 :: (Typeable a, Typeable b) => FunctionIdent -> (a -> b) -> Exp w a -> Exp w b
function1 ident f x = Call (FCall ident) (FTH $ Do . f) (T1 x)

functionHS1 :: (Typeable a, Typeable b) => FunctionIdent -> (a -> HS w b) -> Exp w a -> Exp w b
functionHS1 ident f x = Call (FCall ident) (FTH f) (T1 x)

function2 ::(Typeable a, Typeable b, Typeable c) => FunctionIdent -> (a -> b -> c) -> Exp w a -> Exp w b -> Exp w c
function2 ident f x y = Call (FCall ident) (FTH $ Do . uncurry f) (T2 (x,y))

functionHS2 ::(Typeable a, Typeable b, Typeable c) => FunctionIdent -> (a -> b -> HS w c) -> Exp w a -> Exp w b -> Exp w c
functionHS2 ident f x y = Call (FCall ident) (FTH $ uncurry f) (T2 (x,y))

function3 ::(Typeable a, Typeable b, Typeable c, Typeable d) => FunctionIdent -> (a -> b -> c -> d) -> Exp w a -> Exp w b -> Exp w c -> Exp w d
function3 ident f x y z = Call (FCall ident) (FTH $ Do . uncurryN f) (T3 (x,y, z))

function4 ::(Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => FunctionIdent -> (a -> b -> c -> d -> e) -> Exp w a -> Exp w b -> Exp w c -> Exp w d -> Exp w e
function4 ident f x y z a = Call (FCall ident) (FTH $ Do . uncurryN f) (T4 (x,y, z, a))

unary :: (Typeable a, Typeable b) => FunctionIdent -> (a -> b) -> Exp w a -> Exp w b  
unary ident f x = Call (FUnary ident) (FTH $ Do . f) (T1 x)

reference :: Typeable a => Exp w a -> Exp w (CPtr a)
reference = Call (FUnary "&") (FTH Create) . T1

binary :: (Typeable a, Typeable b, Typeable c) => FunctionIdent -> (a -> b -> c) -> Exp w a -> Exp w b -> Exp w c 
binary ident f x y = Call (FBinary ident) (FTH $ Do . uncurry f) (T2 (x,y))

binaryHS :: (Typeable a, Typeable b, Typeable c) => FunctionIdent -> (a -> b -> HS w c) -> Exp w a -> Exp w b -> Exp w c 
binaryHS ident f x y = Call (FBinary ident) (FTH $ uncurry f) (T2 (x,y))

unsafeDeref :: (PtrType t, Typeable a) => Exp w (Ptr t a) -> Exp w a
unsafeDeref x = Call (FUnary "*") (FTH $ Read id) (T1 x)

while :: Typeable a => (Exp w a -> Exp w Bool) -> (Exp w a -> Exp w a) -> Exp w a -> Exp w a
while = While 

eMap :: (Typeable a, Typeable b) => (Exp w a -> Exp w b) -> Exp w [a] -> Exp w [b]
eMap f xs = ForAll xs f

eInit :: (Iterable xs, Typeable (Elem xs)) => Exp w xs -> Exp w xs
eInit = ManipulateList (DeclineEnd $ value 1)

eReverse :: (Iterable xs, Typeable (Elem xs)) => Exp w xs -> Exp w xs
eReverse = ManipulateList Reverse

eDrop :: (Iterable xs, Typeable (Elem xs)) => Exp w Int -> Exp w xs -> Exp w xs
eDrop n = ManipulateList (AdvanceBegin n)

eTake :: (Iterable xs, Typeable (Elem xs)) => Exp w Int -> Exp w xs -> Exp w xs
eTake n = ManipulateList (AdvanceEndFromBegin n)

mkFunction0 :: Typeable a => FunctionIdent -> Exp w a -> Exp w a
mkFunction0 i f = WrapFunction (\_ -> f) (Just i) T0

mkFunction1 :: (Typeable a, Typeable b) => FunctionIdent -> (Exp w a -> Exp w b) -> Exp w a -> Exp w b
mkFunction1 i f = tupleCurry1 (WrapFunction (tupleUncurry1 f) (Just i))

mkFunction2 :: (Typeable a, Typeable b, Typeable c) => FunctionIdent -> (Exp w a -> Exp w b -> Exp w c) -> Exp w a -> Exp w b -> Exp w c
mkFunction2 i f = tupleCurry2 (WrapFunction (tupleUncurry2 f) (Just i))

mkFunction3 :: (Typeable a, Typeable b, Typeable c, Typeable d) => FunctionIdent
                                                    -> (Exp w a -> Exp w b -> Exp w c -> Exp w d)
                                                    -> Exp w a -> Exp w b -> Exp w c -> Exp w d
mkFunction3 i f = tupleCurry3 (WrapFunction (tupleUncurry3 f) (Just i))

wrapFunction0 :: (Typeable b) => Exp w b -> Exp w b
wrapFunction0 f = WrapFunction (\_ -> f) Nothing T0



wrapFunction1 :: (Typeable a, Typeable b) => (Exp w a -> Exp w b) -> Exp w a -> Exp w b
wrapFunction1 f = tupleCurry1 (WrapFunction (tupleUncurry1 f) Nothing )

wrapFunction2 :: (Typeable a, Typeable b, Typeable c) => (Exp w a -> Exp w b -> Exp w c) -> Exp w a -> Exp w b -> Exp w c
wrapFunction2 f = tupleCurry2 (WrapFunction (tupleUncurry2 f) Nothing)

wrapFunction3 :: (Typeable a, Typeable b, Typeable c, Typeable d) => (Exp w a -> Exp w b -> Exp w c -> Exp w d)
                                                      ->  Exp w a -> Exp w b -> Exp w c -> Exp w d
wrapFunction3 f = tupleCurry3 (WrapFunction (tupleUncurry3 f) Nothing)

functionPtr1 f = FunctionPtr (tupleUncurry1 f)
functionPtr2 f = FunctionPtr (tupleUncurry2 f) 

