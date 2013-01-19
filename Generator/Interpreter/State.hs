{-# LANGUAGE ExistentialQuantification #-}
module Generator.Interpreter.State where
import Data.IntMap (IntMap, toList)
import qualified Data.IntMap as I

data Var a  = Var Int
data State w = State 
    { classes :: IntMap Opaque
    , next    :: Int 
    , world   :: w
    }

instance Show w => Show (State w) where
    show s =  "State { classes = Count(" ++ (show . length . toList $ classes s) ++ "), "
                    ++ "next = " ++ show (next s) ++ ", "
                    ++ "world = " ++ show (world s) ++ ", "
                    ++ " }"

defaultState = State { classes = I.empty, next = 1, world = error "no world set" }

data Opaque = forall a . Opaque a
instance Show Opaque where
    show _ = "O"

