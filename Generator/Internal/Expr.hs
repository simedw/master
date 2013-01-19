{-# LANGUAGE GADTs, ScopedTypeVariables, RebindableSyntax,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts, IncoherentInstances,
  FlexibleInstances, UndecidableInstances, Rank2Types, OverlappingInstances, TypeOperators #-}
{-# LANGUAGE DatatypeContexts #-}
module Generator.Internal.Expr 
  ( Exp(..)
  , HS(..)
  , Var(..)
  , FunctionType(..)
  , FunctionKind(..)
  , ProMonad(..)
  , Magic(..)
  , CTuple(..)
  , FPtr(..)
  , Basetype
  , FunctionIdent
  , IteratorManipulator(..)
  , (:<:)(..)
  , inspect --  Exp a -> String
  ) where

import Generator.Interpreter.State (State)
import Generator.Internal.Types 
import Generator.Types (CPtr)

import Control.Applicative ((<$>))

import qualified Prelude as P
import Prelude
       (Eq(..), String, error, Bool(..), Int, (.), ($), Show(..),
       undefined, (++), (&&), not, Maybe)

import Data.Either
import Data.Foldable

type FunctionIdent = String

-- | Describing what kind of function we are using 
data FunctionKind 
  = FBinary FunctionIdent -- ^ binary operations such as '+'
  | FUnary FunctionIdent  -- ^ unary operations such as negation '-'
  | FCall FunctionIdent   -- ^ call a normal function 
  | FMethod FunctionIdent Bool -- ^ call a method, True if we should use (->) 
  | forall c . Class c => FStatic c FunctionIdent -- ^ call to method on singleton
  | forall c . Class c => FInitClass c -- ^ creating a new class instances

instance Show FunctionKind where
    show (FBinary id) = "FBinary(" ++ show id ++ ")"
    show (FUnary  id) = "FUnary(" ++ show id ++ ")"
    show (FCall   id) = "FCall(" ++ show id ++ ")"
    show (FMethod id True)  = "FMethod(" ++ show id ++ ", Ptr)"
    show (FMethod id False) = "FMethod(" ++ show id ++ ", NoPtr)"
    show (FStatic c id) = "FStatic(" ++ show (classname c) ++ "::" ++ show id ++ ")"
    show (FInitClass  c) = "FInitClass(" ++ show (classname c) ++ ")"

-- | Basically an Either type for deciding if we want to evaluate things
-- using the 'Exp' values or not
data FunctionType a b = FTH a | PureH b

-- | Tuple is a way to uncurry functions.
-- New and larger instances may be added when needed
class CTuple f where
  type TInternal f 
  tupleRun   :: (forall b w. Typeable b => state w -> Exp w b 
                                                   -> (b,state w)) -> state w 
                                                   -> f w -> (TInternal f, state w )
  tupleFold  :: P.Monad m => (forall a w . Typeable a => Exp w a -> m ([x], y)) 
                                                      -> f w -> m ([x],[y])
  tupleMap   :: (forall a w . Exp w a -> Exp w a) -> f w -> f w 
  mkTuple    :: P.Monad m => f w -> m Var -> m (f w) 
  tupleTypes :: f w -> (forall a . Typeable a => a -> CType) -> [CType]
  back    :: TInternal f -> f w
  replace :: (forall a . [(Var,Var)] -> [Var] 
                                     -> Exp w a -> Exp w a) 
                                     -> [(Var,Var)] -> f w -> f w
  tupleSize  :: f w -> Int
  tupleTrans :: P.Monad m =>  f w 
                          -> (forall b w. Typeable b => Exp w b -> m ([Var], stmt)) 
                          -> m (f w, [stmt])

-- | Variables to allow compilation of expressions that we not yet have
-- the 'FunctionIdent' is set if we generate the variable inside a function
data Var = Var Int (Maybe FunctionIdent) | VarBottom
  deriving (Eq, Show)

-- | Convert from one typeable to another without change the underlaying value
type family Basetype a
type instance Basetype (Const a) = Basetype a
type instance Basetype (Ref   a) = Basetype a
type instance Basetype (Cls (Iter a)) = Basetype (CPtr (Elem a))
type instance Basetype (Ptr t a)      = Basetype a


-- | The main datatype that is both runnable and compileable
data Exp w a where
    Prim         :: Typeable a => a -> Exp w a  
    Let          :: Exp w a -> (Exp w a -> Exp w b) -> Exp w b 
                                                
    Variable     :: Var     -> Exp w a
    Return       :: a -> Exp w a
    Call         :: (Typeable r, CTuple a) => FunctionKind 
                                           -> FunctionType  (TInternal a  -> HS w r)
                                                            (a w          -> HS w r)
                                           -> a w -> Exp w r 
    WrapFunction :: (CTuple f, Typeable r) => (f w -> Exp w r) 
                                           -> Maybe FunctionIdent -> f w -> Exp w r
    FunctionPtr  :: (CTuple f, Typeable r) => (f w -> Exp w r) -> Exp w (FPtr f r)
    If           :: Typeable a => Exp w Bool -> Exp w a -> Exp w a -> Exp w a
    While        :: Typeable a => (Exp w a -> Exp w Bool) 
                               -> (Exp w a -> Exp w a) 
                               -> Exp w a
                               -> Exp w a
    ForAll       :: (Typeable a, Typeable b) => Exp w [a]
                                       -> (Exp w a -> Exp w b)
                                       -> Exp w [b]
    UseList      :: (Typeable (Elem xs), Iterable xs) => Exp w xs -> Exp w (Cls xs)
    UseIterable  :: (Typeable (Elem xs), Iterable xs) => Exp w (Cls xs) -> Exp w xs
    ManipulateList :: (Iterable xs, Typeable (Elem xs)) => IteratorManipulator w 
                                                        -> Exp w xs -> Exp w xs
    FMap         :: (Typeable a, Typeable b, Basetype a ~ Basetype b)
                 => (a -> HS w b) -> Exp w a -> Exp w b
    IterMap      :: (Iterable as, Iterable bs, Basetype (Elem as) ~ Basetype (Elem bs))
                 => (Elem as -> HS w (Elem bs)) -> Exp w as -> Exp w bs
    

-- | For describing iteration manipulation 
-- used to preform various optimizations (reverse . reverse == reverse)
data IteratorManipulator w = Reverse                       -- ^ @reverse@
                         | AdvanceBegin        (Exp w Int) -- ^ @drop x@
                         | DeclineEnd          (Exp w Int) -- ^ @reverse . drop x . reverse@ 
                         | AdvanceEndFromBegin (Exp w Int) -- ^ @take x@
                         | DeclineBeginFromEnd (Exp w Int) -- ^ @reverse . take x . reverse@

-- 'HS' contains computation regarding the internal heap
data HS w a where
    Do     :: a -> HS w a
    Then   :: HS w a -> (a -> HS w b) -> HS w b
    DoExp  :: Exp w a -> HS w a
    Read   :: (c -> a) -> Ptr t c -> HS w a
    Write  :: (c -> c) -> Ptr t c -> HS w ()
    Create :: c -> HS w (Ptr t c)
    Get    :: HS w w
    Put    :: w -> HS w ()


inspect x = case x of
        If {}             -> "If"
        Let a f          -> "Let(" ++ "Exp w a" ++ " -> " ++ inspect (f a) ++ ")"
        Return {}         -> "Return"
        While {}          -> "While"
        WrapFunction {}   -> "WrapFunction"
        Call kind _ _     -> "Call<" ++ show kind ++ ">"
        FunctionPtr {}    -> "FunctionPtr"    
        ForAll {}         -> "Forall"
        ManipulateList {} -> "ManipulateList"
        Variable {}       -> "Variable"
        UseList {}        -> "UseList"
        Prim {}           -> "Prim"

infixl 1 >>= 

-- A Monad with Typeable constraints on return and 'Magic' on bind
class ProMonad m where
    return :: Typeable a => a    -> m a
    (>>=)  :: m a  -> (Magic (m a) -> m b) -> m b
    fail   :: String -> m a

instance ProMonad (Exp w) where
    return = Prim
    x >>= f  = Let x f
    fail   = error "ProMonad failed!"

instance ProMonad (HS w) where
    return = Do
    (>>=) = Then
    fail   = error "ProMonad failed!"

type family Magic a
type instance Magic (Exp w b) = Exp w b
type instance Magic (HS w b) = b

instance Typeable a => Typeable (Exp w a) where 
    toCType _ = toCType (undefined :: a)

-- | Coresponds to a function pointer in C++
data FPtr a b = FPtr { unFun :: TInternal a -> b }

instance (CTuple a, Typeable b) => Typeable (FPtr a b) where 
  toCType  _ =  error "toCType on FPtr"
  toCValue _ =  error "toCValue on FPtr"

-- | a :<: b, b should be more constrained than a 
-- used to automatically remove certain constructors
class (Typeable a, Typeable b) => a :<: b where 
    promote :: Exp w a -> Exp w b
    demote  :: Exp w b -> Exp w a

instance (Typeable a) => a :<: a where 
    promote a = a
    demote  a = a

instance Typeable a => a :<: (Const a) where 
    promote = FMap $ return . Const
    demote  = FMap $ return . unConst

instance Typeable a => a :<: (Ref a) where 
    promote = FMap $ return . Ref
    demote  = FMap $ return . unRef

ptrToConst :: (PtrType t, Typeable a) => Exp w (Ptr t a) -> Exp w (Ptr t (Const a))
ptrToConst = FMap (\a -> Read Const a >>= Create ) 
ptrFromConst :: (PtrType t, Typeable a) => Exp w (Ptr t (Const a)) -> Exp w (Ptr t a)
ptrFromConst = FMap (\a -> Read unConst a >>= Create ) 

instance (PtrType t, Typeable a) => Ptr t a :<: Ptr t (Const a) where 
    promote = ptrToConst 
    demote  = ptrFromConst 

