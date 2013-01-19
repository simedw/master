{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, Rank2Types
  , OverlappingInstances, TypeFamilies, UndecidableInstances, IncoherentInstances
  , GADTs #-}
module Generator.Internal.Types
 ( CValue
 , CType
 , Typeable(..)
 , PtrType(..)
 , InternalPtr
 , Iterable(..)
 , Class(..)
 -- * C++ Types
 , Cls(..)
 , Const(..)
 , Ref(..)
 , Ptr(..)
 , Iter(..)
 )
 where

import Generator.Interpreter.State
import Data.List (intercalate)

-- | Represents how the value would be written in C++
type CValue = String

-- | Represents the type of a value in C++
type CType  = String 

-- | Values which can be represented in C++
class Typeable a where
  -- | Gives a C++ Values
  --
  -- >>> toCValue True 
  -- "true"
  --
  toCValue :: a -> CValue
  -- | Gives a C++ Type 
  -- the argument maybe be undefined
  --
  -- >>> toCType (undefined :: Bool)
  -- "bool"
  --
  toCType  :: a -> CType



-- | Used to accommodate different types of pointers
-- such as vanilla C (*), std::shared_ptr...
class PtrType typ where
    wrapTypeable :: typ -> CType -> CType

-- | A pointer to a heap value or a null pointer
-- typ has to be a member of 'PtrType'
data Ptr typ a = Ptr Int | NullPtr

instance Show (Ptr typ a) where
    show (Ptr i) = "Ptr<" ++ show i ++ ">"
    show NullPtr = "NullPtr"

-- | Pointer for internal use only, no C++ version
type InternalPtr = Ptr InternalPtrType
data InternalPtrType = InternalPtr

instance PtrType InternalPtrType where
    wrapTypeable _ = error "Cannot write the C++ type of InternalPtr"



-- | Values that corresponds to Classes
class Class a where 
   -- | Gives the C++ namespace for a certain class 
   -- The argument may be undefined
   --
   -- >>> namespace (undefined :: Str)
   -- ["std"]
   --
   namespace :: a -> [String]
   -- | Gives the C++ classname for a class 
   -- The argument may be undefined
   --
   -- >>> classname (undefined :: Str)
   -- "string"
   --
   classname :: a -> String

-- | 'Cls' is used to reference to a certain instance of a class 
data Cls a = Cls (InternalPtr a)

instance Show (Cls a) where
    show (Cls ptr) = "Cls (" ++ show ptr ++ ")"

-- | Represents a const value in C++
data Const a = Const { unConst :: a }

instance Eq a => Eq (Const a) where
    (Const s1) == (Const s2) = s1 == s2
    (Const s1) /= (Const s2) = s1 /= s2


instance Eq a => Eq (Ref a) where
    (Ref s1) == (Ref s2) = s1 == s2
    (Ref s1) /= (Ref s2) = s1 /= s2



-- | Represents a reference value in C++
data Ref   a = Ref   { unRef :: a } 

-- | Class for Iterable containers 
class (Class a, Typeable a)  => Iterable a where
    -- | 'Elem' is the type of the value inside the container
    type Elem a
    beginIter :: a -> String
    beginIter _ = "begin"
    endIter   :: a -> String
    endIter   _ = "end"
    frontElem :: a -> String
    frontElem _ = "front"
    backElem :: a -> String
    backElem _ = "back"
    contents  :: a -> [Elem a]
    buildIter :: [Elem a] -> a

-- | An iterator to a container 'a'
data (Iterable a) => Iter a = Iter ([Elem a], Int)

instance (Iterable a, Eq (Elem a)) => Eq (Iter a) where
    (Iter p1) == (Iter p2) = p1 == p2

instance Typeable a => Class (Iter a) where
    classname _ = toCType (undefined :: a) ++ "::iterator"
    namespace _ = []

-- | An instance of a class is always typeable but doesn't have a value
instance Class c => Typeable (Cls c) where
    toCValue x = error $ "toCValue on Class(" ++ toCType x ++ ")"
    toCType  _ = let (x :: c) = undefined in intercalate "::" $ namespace x ++ [classname x]

-- | An pointer is always typeable but doesn't have a value
instance (Typeable a, PtrType typ) => Typeable (Ptr typ a) where
    toCType  _  = let (x :: a) = undefined in wrapTypeable (undefined :: typ) $ toCType x
    toCValue (p@NullPtr) = toCType p ++ "()/*NULL*/"
    toCValue x  = error $ "toCValue on Ptr(" ++ toCType x ++ ")"
instance Typeable a => Typeable (Const a) where 
    toCType (_ :: Const a)  = "const " ++ toCType (undefined :: a)-- ++ " &"
    toCValue (Const a) = toCValue a

instance Typeable a => Typeable (Ref a) where 
    toCType (_ :: Ref a)  = toCType (undefined :: a) ++ " &"
    toCValue (Ref a) = toCValue a


instance Class c => Typeable (Const (Cls c)) where
    toCValue x = error $ "toCValue on Class(" ++ toCType x ++ ")"
    toCType  _ = let (x :: c) = undefined in "const " ++ 
                intercalate "::" (namespace x ++ [classname x]) -- ++ " &"

instance Class a => Class (Cls a) where
    namespace _ = namespace (undefined :: a)
    classname _ = classname (undefined :: a)


