{-# LANGUAGE GADTs , FlexibleInstances, TypeFamilies, FlexibleContexts
  #-}

{-# LANGUAGE ExistentialQuantification, RebindableSyntax, TypeSynonymInstances #-}

module AssertionRule.Rule
  ( Rule
  , assert 
  , avoid
  , store -- :: Exp a -> Rule (Exp a)
  , precon 
  , exec 
  , runRule
  ) where

import qualified Generator.Expr as E
import Generator.Expr hiding (ifThenElse)
import Generator.Prelude
import qualified Prelude as P
import qualified Generator.Internal.Expr as I
import Generator.Types
import Generator.Internal.Types
import Generator.Internal.Tuple

data Rule w a where  
    Bind   :: Rule w a -> (a -> Rule w b) -> Rule w b
    Return :: Typeable a => a -> Rule w a
    Store  :: Exp w a -> Rule w (Exp w a)
    Assert :: Bool -> Exp w Bool -> Rule w Bool

runRule x = case x of
    Assert r x `Bind` f -> if x then runRule (f $ True) else return r
    Return x `Bind` f -> runRule (f $ x)
    Store  x `Bind` f -> x >>= \y -> runRule (f y)
    (ma `Bind` f) `Bind` g -> runRule $ ma `Bind` \x -> (f x `Bind` g)
    Return x -> return x
    Assert r x -> x || return r
    _ -> P.error (show x)

instance Show (Rule w a) where 
    show x = case x of
       Bind{}   -> "Bind"
       Return{} -> "Return"
       Store{}  -> "store"
       Assert{} -> "assert"

type instance I.Magic (Rule w a) = a

instance ProMonad (Rule w) where 
    return = Return
    (>>=)  = Bind 

assert = Assert False 
avoid  = Assert False . not
store :: Exp w a -> Rule w (Exp w a)
store  = Store -- Return 
precon = Assert True 
exec   = store

