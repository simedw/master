{-# LANGUAGE GADTs #-}
module Generator.Compiler.AST where

import Generator.Internal.Types

type Var    = Int

-- A typed expression
data TExp = (:::) Exp CType
  deriving (Show, Eq)
    
unTExp :: TExp -> Exp
unTExp (exp ::: _) = exp


data Stmt
  = SAssignment Bool Var TExp -- ^ (Bool ? "VarType" : "") Var = TExp;
  | SFunctionPtr Var [CType] CType Exp
  | SDecl Var CType          -- ^ CType Var;
  | SIf Exp Stmt Stmt         -- ^ if (Exp) then Stmt else Stmt
  | SWhile Exp Stmt           -- ^ While (Exp == True) {Stmt}
  | SExpr TExp                -- ^ TExp;
  | SList [Stmt]              -- ^ Sequential statements
  | SReturn Exp               -- ^ return exp;
  | SNop                      -- ^ nothing
  | SFunc CType String [(CType, Var)] Stmt Bool -- ^ CType String(CType v1, CType v2, ...) { Stmt }
  | SComment String           -- ^ /* String */
  | SClass CType String [(CType, Var)] [(CType, Var)] Stmt
  deriving (Show, Eq)

data Exp 
  = EValue CValue -- ^ A concret C++ value
  | EVar Var      -- ^ A variable, negative numbers are either void or errors
  | ECall [String] String [Exp] -- ^ Call function with namespace, name and arguments
  | EMethod Var [String] String [Exp] Bool -- ^ Call method with namespace name, 
                                           -- arguments and a bool (True =>
                                           -- '->', False => '.')
  | EBin String Exp Exp -- ^ Binary function call
  | EUnary String Exp   -- ^ Unary function Call
  | EDeref Var          -- ^ Derefering of a variable
  deriving (Show, Eq)

type ReturnType    = String
type FunctionName  = String
type Argument      = String
data TypedFunction = TP ReturnType [Argument] Stmt
    deriving Show

