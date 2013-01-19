{-# LANGUAGE ExistentialQuantification #-}
module AssertionRule.AssertionRule
  ( Assertion(..)
  , Rule
  , rule 
  , assert 
  , avoid
  , store -- :: Exp a -> Rule (Exp a)
  , precon 
  , exec 
  , runRule
  ) where 

import AssertionRule.Precon
import AssertionRule.Rule
import Generator.Expr

data Assertion w = Assertion String PreCon (Exp w Bool)

instance Show (Assertion w) where 
    show (Assertion s c r) = "Command:" ++ show c ++ " => " ++ s

rule name precon exp = Assertion name (PCommand precon) (runRule exp)
