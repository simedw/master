{-# LANGUAGE ExistentialQuantification, GADTs #-}
module AssertionRule.Precon
  ( PreCon(..) 
  , Command(..)
  , Target(..)
  , target
  , cmd
  ) where

data CommandT = forall x . (Show x, Command x) => T x

instance Show CommandT where
    show (T x) = show x 

instance Show PreCon where 
    show x = case x of
        PAnd a b -> show a ++ " and " ++ show b
        PNot a -> "not (" ++ show a ++ ")"
        PCommand c -> show c
        PTarget t -> show t

data PreCon where
    PAnd :: PreCon -> PreCon -> PreCon
    PNot :: PreCon -> PreCon
    PCommand :: Command a => a -> PreCon
    PTarget  :: Target  a => a -> PreCon

class Show a => Command a where
    renderC :: a -> String

class Show a => Target  a where 
    renderT :: a -> String

{- combinators -}

target :: Target a => a -> PreCon
target = PTarget

cmd    :: Command a => a -> PreCon
cmd    = PCommand


