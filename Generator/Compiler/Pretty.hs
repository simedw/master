module Generator.Compiler.Pretty where

import Generator.Compiler.AST
import Control.Monad
import Text.PrettyPrint
import Data.List (intercalate, nub)

newline :: Doc 
newline = vcat [empty]

a <=> b = a <+> equals <+> b
braces' a = lbrace $+$ nest 4 a $$ rbrace

pintercalate p [] = empty
pintercalate p (v : []) = v
pintercalate p (v : vs) = v <> p <> pintercalate p vs

pStmt :: Stmt -> Doc
pStmt stm = case stm of
  SAssignment True var (exp ::: typ) -> 
    text typ <+> pStmt (SAssignment False var (exp ::: typ))
  SAssignment False (-1) (exp ::: _) ->
    case exp of
        EVar v -> {-empty-} text "//Assignment of a variable to void"
        e      -> pExp exp <> semi
  SAssignment False var (exp ::: _) -> 
    pVar var <+> equals <+> pExp exp <> semi
  SFunctionPtr var types ret val ->
    text ret <+> parens (text "*" <> pVar var) 
             <+> parens (text $ intercalate ","  $ types) 
             <+> equals <+> pExp val <+> semi; 
  SDecl var typ -> 
    text typ <+> pVar var <> semi
  SIf p tru fal -> 
    text "if" <> parens (pExp p) <+> lbrace $$ 
    nest 4 (pStmt tru) $$ rbrace <+> text "else" <+> lbrace $$ 
    nest 4 (pStmt fal) $$ rbrace
  SWhile p body -> 
    text "while" <> parens (pExp p) <+> lbrace $$ 
    nest 4 (pStmt body) $$ rbrace 
  SExpr (exp ::: typ)   -> pExp exp <> semi
  SList smts            -> vcat $ map (<> empty) (map pStmt smts)
  SReturn exp           -> text "return" <+> pExp exp <> semi
  SNop                  -> empty
  SFunc ret name args stmt isConst ->
    text ret <+> text name <>
    parens (hcat . punctuate comma $ map (\(typ,v) -> text typ <+> pVar v) args) <+>
    (if isConst then text "const" else empty) <+>
    lbrace $$ nest 4 (pStmt stmt) $$ rbrace
  SClass ret name args largs stmt -> 
    text "class" <+> text name $$ braces' (
        text "public:" $$ nest 4 (
        text name <> argList args <+> colon <+> memberAss <+> braces' empty $$
        text ret <+> text "operator()" <> argList largs 
                 <+> text "const" $$ braces' (pStmt stmt)) $$
        text "private:" $$ nest 4 members
        
    ) <> semi
    where 
      memberAss = pintercalate (text ",") $ map (\(typ,v) -> pVar v <> parens (pVar v)) args
      members = vcat (map (\(typ,v) -> text typ <+> pVar v <> semi) args)
      argList arg =  parens (hcat . punctuate comma $ 
                             map (\(typ,v) -> text typ <+> pVar v) arg
                            )
  SComment cmt -> text "/*" <+> text cmt <+> text "*/"

pExp :: Exp -> Doc
pExp exp = case exp of
  EValue value                -> text value
  EVar var                    -> pVar var
  ECall namespace fident args -> 
    pFunction namespace fident <> lparen <> pArgs args <> rparen
  EMethod var namespace [] args _ -> 
    pVar var <> pFunction namespace "" <> lparen <> pArgs args <> rparen
  EMethod var namespace fident args False -> 
    pVar var <> text "." <> pFunction namespace fident <> lparen <> pArgs args <> rparen
  EMethod var namespace fident args True -> 
    pVar var <> text "->" <> pFunction namespace fident <> lparen <> pArgs args <> rparen

  EBin fident a b -> parens $ pExp a <+> text fident <+> pExp b
  EUnary fident a -> text fident <> pExp a
  EDeref var -> text "*" <> pVar var

pVar :: Var -> Doc
pVar (-1) = empty -- Void is implemened as -1, but since variables "can't" be voids
pVar i | i < 0 = text "<pVar:" <> int i <> text ">"
pVar i = text $ "_" ++ names !! i
    where names = [1..] >>= flip replicateM ['a'..'z']
pFunction  :: [String] -> String-> Doc
pFunction [] f      = text f
pFunction ([]:xs) f = pFunction xs f
pFunction (x:xs) f  = text x <> colon <> colon <> pFunction xs f

pArgs :: [Exp] -> Doc
pArgs []     = empty
pArgs (x:[]) = pExp x
pArgs (x:xs) | isEmpty (pExp x) = pArgs xs
pArgs (x:xs) | otherwise        = pExp x <> comma <+> pArgs xs

prettyPrint =  render . pStmt 

prettyFunction :: FunctionName -> TypedFunction -> String
prettyFunction name (TP ret args body) = render $ pref $$ nest 4 code $$ suff
  where code = pStmt body
        pref = text ret <+> text name 
                        <> parens (hcat . punctuate comma $ map text args) 
                        <+> lbrace
        suff = rbrace

