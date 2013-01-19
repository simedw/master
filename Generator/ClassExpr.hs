{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies,
OverlappingInstances, IncoherentInstances, UndecidableInstances, ViewPatterns
 , FlexibleInstances, TypeOperators, MultiParamTypeClasses, FunctionalDependencies  #-}
{-# LANGUAGE UndecidableInstances #-}
module Generator.ClassExpr
 ( initClass0
 , initClass1
 , initClass2
 , copyClass
 , singleton0
 , create
 , createC
 , createPtr
 , read
 , readExp
 , readPtr
 , write
 , writeExp
 , writePtr
 , method0
 , method1
 , method2
 , method3
 , method4
 , staticMethod
 , staticMethod0
 , useList, useIter
 , (:<<:)(..)
 , IsClass(..)
 , IsPtr(..)
 ) where

import Generator.IsPtr
import Generator.Internal.Expr
import Generator.Internal.Tuple
import Data.Tuple.Curry
import Generator.Expr
import Generator.Internal.Types
import Generator.Types
import Prelude hiding (return, read, (>>=))


create :: (Class c) => c -> HS w (Cls c)
create x = Create x >>= \(ptr :: InternalPtr c) -> (return $ Cls ptr)


createC :: (Class c) => c -> HS w (Const (Cls c))
createC x = Create x >>= \(ptr :: InternalPtr c) -> (return . Const $ Cls ptr)

createPtr :: c -> HS w (Ptr t c)
createPtr = Create


readExp :: (c -> a) -> Exp w (Cls c) -> HS w a
readExp f e = DoExp e >>= \(Cls p) -> Read f p

read :: (c -> a) -> Cls c -> HS w a
read f (Cls p) = Read f p

readPtr :: PtrType t => (c -> a) -> Ptr t (Cls c) -> HS w a
readPtr f ptr = Read id ptr >>= read f

writeExp :: (c -> c) -> Exp w (Cls c) -> HS w ()
writeExp f e = DoExp e >>= \(Cls p) ->  Write f p

write :: (c -> c) -> Cls c -> HS w ()
write f (Cls p) = Write f p

writePtr :: (c -> c) -> Ptr t (Cls c) -> HS w ()
writePtr f ptr = Read id ptr >>= write f


initClass2  :: (Typeable a, Typeable b, Class c) =>
               (a -> b -> c) -> Exp w a -> Exp w b -> Exp w (Cls c)
initClass2 (f :: a -> b -> c) x y =
    Call (FInitClass (undefined :: c))
         (FTH $ \(a,b) -> Create (f a b) >>= \(ptr :: InternalPtr c) -> (Do $ Cls ptr))
         (T2 (x, y))

initClass1 :: (Typeable a, Class c) => (a -> c) -> Exp w a -> Exp w (Cls c)
initClass1 (f :: a -> c) x = Call (FInitClass (undefined :: c)) (FTH $ \a -> Create (f a) >>= \(ptr :: InternalPtr c) -> (Do $ Cls ptr)) (T1 x)
--initClass f x = InitClass (D .  f) |$| x

initClass0 :: Class c => c -> Exp w (Cls c)
initClass0 (f::c) = Call (FInitClass (undefined :: c)) (FTH $ \_ -> Create f >>= \(ptr :: InternalPtr c) -> (Do $ Cls ptr)) (T0)


copyClass :: (Class c, Cls c :<<: a) => Exp w a -> Exp w (Cls c)
copyClass (prepare -> p) = 
    Call (FInitClass (help p)) (PureH $ tupleUncurry1 $ DoExp) (T1 p)
    where help (a :: Exp w (Cls c)) = undefined :: c

class (Typeable a, Typeable b) => a :<<: b | b -> a where 
    prepare :: Exp w b -> Exp w a
    wasPtr :: Exp w b -> Bool

instance (IsPtr (Exp w (Const a)), Typeable a, b :<<: a) => b :<<: Const a where
    prepare = prepare . deConst
    wasPtr  = wasPtr . deConst
--instance (Typeable a, PtrType t) => Ptr t a :<<: Ptr t (Const a) where 
instance (IsPtr (Exp w (Ref a)), Typeable a, b :<<: a) => b :<<: Ref a  where
    prepare = prepare . rmRef
    wasPtr  = wasPtr . rmRef

instance (IsPtr (Exp w (Cls c)), Class c) => Cls c :<<: Cls c where
    prepare a = a
    wasPtr  _ = False

instance (Typeable c, PtrType t) => Ptr t c :<<: Ptr t c where 
    prepare a = a
    wasPtr  _ = True

--instance (ClassM a, UnConst a :<: a) => ClassM a 

class IsClass a where
  type BottomCls a
instance Class c => IsClass (Cls c) where
  type BottomCls (Cls c) = c
instance (PtrType t, IsClass c) => IsClass (Ptr t c) where
  type BottomCls (Ptr t c) = BottomCls c
instance IsClass c => IsClass (Const c) where
  type BottomCls (Const c) = BottomCls c
instance IsClass c => IsClass (Ref c) where
  type BottomCls (Ref c) = BottomCls c

method0 :: (IsClass x, x :<<: c, Typeable b)
        => FunctionIdent -> Exp w c -> (x -> HS w b) -> Exp w b
method0 ident ptr f = Call (FMethod ident (wasPtr ptr)) 
                             (FTH f) 
                             (T1 (prepare ptr))

method1 :: (IsClass x, Typeable a, x :<<: c, Typeable b)
        => FunctionIdent -> Exp w c -> (x -> a -> HS w b) -> Exp w a -> Exp w b
method1 ident ptr f x = Call (FMethod ident (wasPtr ptr)) 
                             (FTH $ uncurry f) 
                             (T2 (prepare ptr,x))

method2 :: (IsClass x, Typeable a, x :<<: c, Typeable b, Typeable y)
        => FunctionIdent -> Exp w c -> (x -> a -> y -> HS w b)
        -> Exp w a -> Exp w y -> Exp w b
method2 ident ptr f x y = Call (FMethod ident (wasPtr ptr)) 
                             (FTH $ uncurryN f) 
                             (T3 (prepare ptr,x,y))

method3 :: (IsClass x, Typeable a, x :<<: c
           ,Typeable b, Typeable y, Typeable z)
        => FunctionIdent -> Exp w c -> (x -> a -> y -> z -> HS w b)
        -> Exp w a -> Exp w y -> Exp w z -> Exp w b
method3 ident ptr f x y z = Call (FMethod ident (wasPtr ptr)) 
                             (FTH $ uncurryN f) 
                             (T4 (prepare ptr,x,y,z))


method4 :: (IsClass x, x :<<: c
           ,Typeable a, Typeable b, Typeable z, Typeable y, Typeable h)
        => FunctionIdent -> Exp w c -> (x -> a -> y -> z -> h -> HS w b)
        -> Exp w a -> Exp w y -> Exp w z -> Exp w h -> Exp w b
method4 ident ptr f x y z h = Call (FMethod ident (wasPtr ptr)) 
                             (FTH $ uncurryN f) 
                             (T5 (prepare ptr,x,y,z,h))

staticMethod :: (Typeable a, Class c, Typeable b) => FunctionIdent -> c -> (a -> b) -> Exp w a -> Exp w b
staticMethod ident c f x = Call (FStatic c ident) (FTH $ Do . f) (T1 x)

staticMethod0 :: (Typeable b, Class c) => FunctionIdent -> c -> b -> Exp w b
staticMethod0 ident c f = Call (FStatic c ident) (FTH $ \_ -> Do f) T0

useList :: (Iterable xs, Class xs, Typeable (Elem xs)) => Exp w xs -> Exp w (Cls xs)
useList = UseList

useIter :: (Iterable xs, Class xs, Typeable (Elem xs)) => Exp w (Cls xs) -> Exp w xs
useIter = UseIterable

singleton0 :: (Class c, Class a)  => c -> FunctionIdent -> a -> Exp w (Cls a)
singleton0  c ident x = Call (FStatic c ident) 
                            (FTH $ \_ -> Create x >>= \(ptr :: InternalPtr c) -> (Do $ Cls ptr)) 
                            T0
