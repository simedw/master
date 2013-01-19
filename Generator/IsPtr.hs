{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts,
 OverlappingInstances #-}
module Generator.IsPtr where
import Generator.Internal.Expr
import Generator.Internal.Tuple
import Data.Tuple.Curry
import Generator.Expr
import Generator.Types
import Prelude hiding (return, read, (>>=))

class IsPtr a where 
  isPtr :: a -> Bool

instance IsPtr (Exp w (Ptr t a)) where
  isPtr _ = True

instance IsPtr (Exp w (Cls a)) where 
  isPtr _ = False

instance IsPtr (Exp w a) => IsPtr (Exp w (Const a)) where 
  isPtr (_ :: Exp w (Const a)) = isPtr (undefined :: Exp w a)

instance IsPtr (Exp w a) => IsPtr (Exp w (Ref a)) where 
  isPtr (_ :: Exp w (Ref a)) = isPtr (undefined :: Exp w a)

instance Typeable a => IsPtr (Exp w a) where 
  isPtr _ = False


