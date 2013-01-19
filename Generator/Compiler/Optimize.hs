module Generator.Compiler.Optimize where

import Generator.Compiler.AST
import Generator.Compiler.Translator
import Generator.Types
import Generator.Internal.Types

constFolding :: Stmt -> Stmt
constFolding stmt = case stmt of
  SAssignment b v (exp ::: t) -> SAssignment b v (constFoldingE exp ::: t)
  SIf cond tru fls            -> SIf (constFoldingE cond) (constFolding tru) (constFolding fls)
  SWhile exp body             -> SWhile (constFoldingE exp) (constFolding body)
  SExpr (exp ::: t)           -> SExpr $ constFoldingE exp ::: t
  SList xs                    -> SList $ map constFolding xs
  SReturn exp                 -> SReturn $ constFoldingE exp
  SFunc a b c d x              -> SFunc a b c (constFolding d) x
  SClass a b c d e            -> SClass a b c d $ constFolding e
  x -> x

constFoldingE :: Exp -> Exp
constFoldingE exp = case exp of
    EBin ident (EValue op1) (EValue op2) -> case lookup ident foldingList of
        Nothing -> exp
        Just f  -> EValue $ f op1 op2
    e -> e

foldingList :: [(String, CValue -> CValue -> CValue)]
foldingList = 
 [ ("+", \a b -> show (read a + read b :: Int))
 , ("-", \a b -> show (read a - read b :: Int))
 , ("*", \a b -> show (read a * read b :: Int))
 , ("/", \a b -> show (read a `quot` read b :: Int))
 , ("%", \a b -> show (read a `rem` read b :: Int))
 ]



constProg :: Var -> CValue -> Stmt -> (Stmt, Bool)
constProg var value stmt = case stmt of
   SAssignment b v (exp ::: t) ->
    (SAssignment b v (constProgE var value exp ::: t), v == var)
   SIf exp tru fls -> let
    (rt, ct) = constProg var value tru
    (rf, cf) = constProg var value fls
     in (if ct || cf then SIf exp rt rf
                     else SIf (constProgE var value exp) rt rf
        , ct || cf)

   SWhile exp body -> let (r,c) = constProg var value body 
    in (if not c then SWhile (constProgE var value exp) r
            else SWhile exp body, c)
   SExpr (exp ::: t) -> (SExpr ( constProgE var value exp ::: t), False)
   SList xs   -> {-trace (">>" ++ show (help xs) ++ "<<" ) $-}
                (SList (help xs), or $ map (snd . constProg var value) xs)
     where
       help []     = []
       help (y:ys) = let (r,v) = constProg var value y
                        in r : if not v then help ys else ys
   SReturn exp -> (SReturn (constProgE var value exp), False)              
   x -> (x, False) 
    
constProgE :: Var -> CValue -> Exp -> Exp
constProgE var value exp = {- trace (">>" ++ show exp ++ ", " ++ show var ++ "<<") -}case exp of
  EVar v | v == var -> EValue value
  ECall xs s exps    -> ECall xs s (map (constProgE var value) exps)
  EMethod v xs s exps isPtr -> EMethod v xs s (map (constProgE var value) exps) isPtr
  EBin v e1 e2 -> EBin v (constProgE var value e1) (constProgE var value e2)
  x -> x

constantProg :: Stmt -> Stmt
constantProg (SList xs) = SList $ constantProg' xs
constantProg s = SList $ constantProg' [s]

constantProg' :: [Stmt] -> [Stmt]
constantProg' (SList stmt: []) = constantProg' stmt
constantProg' (stmt:rest) = 
 let res = (case stmt of
       SFunc typ name par s x -> SFunc typ name par (constantProg s) x : constantProg' rest
       SClass a b c d s -> SClass a b c d (constantProg s) : constantProg' rest
       SAssignment _ var (EValue v ::: t) -> stmt : [fst $ constProg var v (SList rest)]
       SIf exp tru fls -> SIf exp (constantProg tru) (constantProg fls) : rest
       SWhile exp body -> SWhile exp (constantProg body ) : rest
       x -> x:rest)
 in head res : constantProg' (tail res)
constantProg' [] = []

optimize :: Stmt -> Stmt 
optimize s = snd $ until (\(old,new) -> old == new) (\(old,new) -> (new, optPass new)) (SNop, s)

optPass s = constFolding . deadCodeElimination . constantProg $ s

deadCodeElimination :: Stmt -> Stmt
deadCodeElimination s = deadCodeElim s s

deadCodeElim :: Stmt -> Stmt -> Stmt
deadCodeElim stmt world = case stmt of
  SAssignment True var _  | count var world == 0 -> SNop
  SAssignment False var (EVar _ ::: _) | count var world == 0 -> SNop
  SAssignment False var e | count var world == 0 -> SExpr e
  SDecl var _ | count var world == 0 -> SNop
  SIf e tr fl -> SIf e (deadCodeElim tr world) (deadCodeElim fl world)
  SWhile exp body -> SWhile exp (deadCodeElim body world)
  SList xs -> SList (map (flip deadCodeElim world) xs)
  SFunc a b c d x -> SFunc a b c (deadCodeElim d d) x
  SClass a b c d e -> SClass a b c d (deadCodeElim e e)
  x -> x 
shead (SList (x:xs)) = x
shead _ = SNop


count :: Var -> Stmt -> Int
count v stmt = case stmt of
  SAssignment _ var (EVar var' ::: _) | var == var' -> 0 
  SAssignment _ _ (e ::: _) -> countE v e
  SIf exp s1 s2             -> countE v exp + count v s1 + count v s2 
  SWhile exp s -> countE v exp + count v s           
  SExpr (e ::: _) -> countE v e                 
  SList xs -> sum $ map (count v) xs
  SReturn exp -> countE v exp          
  SFunc _ _ _ s _ -> count v s
  SClass _ _ _ _ s -> count v s
  _ -> 0 

countE :: Var -> Exp -> Int
countE v exp = case exp of 
  EVar var | v == var -> 1
  ECall _ _ exps -> sum $ map (countE v) exps 
  EMethod var _ _ exps _ -> (if var == v then 1 else 0) + sum (map (countE v) exps)
  EBin _ e1 e2 -> countE v e1 + countE v e2
  EUnary _ e1 -> countE v e1
  EDeref var | var == v -> 1
  _ -> 0
     
