{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, Rank2Types
  , OverlappingInstances, TypeFamilies, UndecidableInstances, IncoherentInstances
  , GADTs #-}
module Generator.Types
  ( 
  -- * DataTypes 
    Str(..)
  , Map(..)
  -- * Pointers 
  , CPtr
  , SharedPtr
  -- * Reexport 
  , module Generator.Internal.Types
  ) where

import Generator.Internal.Types
import Data.List

-- | A std::string String
data Str = Str String
  deriving Eq

---- Pointers ----

-- | A standard C/C++ pointer (*)
type CPtr = Ptr CPtrType
data CPtrType = CPtr
  deriving Show
instance PtrType CPtrType where
    wrapTypeable _ = (++ "*")

-- | std::shared_ptr 
type SharedPtr = Ptr SharedPtrType
data SharedPtrType = SharedPtr 
  deriving Show

---- Typeables ----

instance Typeable Int where
    toCValue = show
    toCType  _ = "int"

instance Typeable Integer where
    toCValue = show
    toCType  _ = "int"

instance Typeable Bool where
    toCValue True  = "true"
    toCValue False = "false"
    toCType  _     = "bool"

instance Typeable () where
    toCType _     = "void"
    -- there is no value in C++ corresponding to void
    toCValue _    = error "toCValue ()"

instance Typeable Char where
    toCType _    = "char" 
    toCValue  x  = "'" ++ [x] ++ "'"

instance Typeable Str where
    toCType _        = "std::string"
    toCValue (Str x) = "std::string(\"" ++ x ++ "\")"

instance Typeable a => Typeable [a] where
    -- | toCValue calles a helpfunction 'mk_list' to create a std::list
    -- could be removed an replaced with push operations instead
    toCValue xs = let (a :: a) = undefined in
        template "mk_list" [toCType a] ++ 
        "(" ++ intercalate "," (toCValue (length xs) : map toCValue xs) ++ ")"
    toCType _   = let (a :: a) = undefined in
        template "std::list" [toCType a]

---- Classes ---

data Map a b = Map {unMap :: [Cls (Const a,b)]}
  deriving Show

instance (Typeable a, Typeable b) => Class (Map a b) where 
  namespace _ = ["std"]
  classname _ = template "map" [toCType (undefined :: a), toCType (undefined :: b)] 
-- | Lists are modeld as std::list
instance Typeable a => Class [a] where
    namespace        _ = ["std"]
    classname (_::[a]) = template "list" [toCType (undefined :: a)]

-- | Tuples are modeled as std::pair 
instance (Typeable a, Typeable b) => Class (a,b) where
    namespace _ = ["std"]
    classname (_::(a,b)) = template "pair" [ toCType (undefined :: a) 
                                           , toCType (undefined :: b) ]


instance (Typeable a, Typeable b) => Typeable (Map a b) where
    toCType _ = template "map" [toCType (undefined :: a), toCType (undefined :: b)] 
    toCValue m@(Map []) = toCType m ++ "() /*Empty*/" 
    toCValue _ = error "toCValue on Map"


instance (Typeable a, Typeable b) => Iterable (Map a b) where
  type Elem (Map a b) = Cls (Const a,b)
  contents x = unMap x
  buildIter xs = Map xs

instance Typeable a => Iterable [a] where
  type Elem [a] = a
  contents x = x
  buildIter xs = xs

-- add angle brackets 
angle str | last str == '>' = angle (str ++ " ") 
angle str | otherwise       = "<" ++ str ++ ">"

-- creates a normal C++ template
template name elems = name ++ angle (intercalate "," elems)



