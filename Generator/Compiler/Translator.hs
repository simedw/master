{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, ScopedTypeVariables,
  ViewPatterns, FlexibleContexts #-}
module Generator.Compiler.Translator
  ( translate      -- Typeable a => String -> Exp w a -> Stmt
  , translateFuncs -- Int -> [Fun] -> [Stmt]
  , nicer          -- Stmt -> Stmt
  , Fun(..)        -- *
  ) where



import Generator.Compiler.AST hiding (Exp, Var)
import qualified Generator.Compiler.AST as AST

import Generator.Internal.Expr hiding (return, (>>=))
import qualified Generator.Internal.Expr as IE

import Generator.Internal.Tuple
import Generator.Internal.Types

import qualified Generator.Prelude as GP

import Control.Monad.Trans.State (runStateT, StateT)
import Control.Monad.State.Class
import Control.Monad.Identity
import Control.Monad
import Control.Applicative
import Data.Tuple.Curry
import Data.Tuple (swap)

import Data.List (intercalate, nub, sort,(\\))
import Data.Maybe (fromMaybe)

-- | For creating new functions
-- First bool defines if it's a Class
-- Second bool if it's a partial function
data Fun = forall w r f . (CTuple f , Typeable r) => 
             Fun (f w -> Exp w r) FunctionIdent [(Var, CType)] Bool Bool

-- | A state to store functions and name supply
data State = State 
    { nameSupply :: [Int]
    , nextClass :: Int
    , nextFun :: Int
    , functions :: [Fun]
    , currentFun :: Maybe String
    }

newtype T a = T { unT :: StateT State Identity a}
    deriving (Monad, MonadState State, Functor)


-- | Run a compilation with the 'defaultState'
runT :: T a -> a
runT = fst . runIdentity . flip runStateT defaultState . unT

-- | Like 'runT' but with an 'Int' for setting the current function count, 
-- useful for creating unique function names
runTS :: Int -> T a -> (a, State)
runTS i =  runIdentity . flip runStateT ( defaultState { nextFun = i } ) . unT

-- | Just a default state with the current function set to "defaultState"
defaultState = State { nameSupply = [0..], nextClass = 0, functions = []
                     , nextFun = 0, currentFun = Just "defaultState" }

-- | Converts 'Var' to Variable in our AST
toAVar :: Var -> AST.Var
toAVar (Var i _) = i
toAVar VarBottom = -1

-- | Translate a list of functions to classes
-- the integer denotes the next function name
translateFuncs :: Int -> [Fun] -> [Stmt]
translateFuncs i f = removeDubFun (translateFuncs' i f)
 where 
    translateFuncs' :: Int -> [Fun] -> [Stmt]
    translateFuncs' _ []     = [] 
    translateFuncs' i (x:xs) = let (j, ys , s) = translateFunc i x 
                           in translateFuncs' j (ys ++ xs) ++ [s]

-- | Creates a new function 
addFun :: (CTuple f, Typeable r) => (f w -> Exp w r) -> Maybe FunctionIdent 
                                 -> [(Var, CType)]   -> T FunctionIdent 
addFun = addFun' False
-- | Creates a new partial functions 
addPartial :: (CTuple f, Typeable r) => (f w -> Exp w r) -> Maybe FunctionIdent 
                                    -> [(Var, CType)]    -> T FunctionIdent 
addPartial = addFun' True

addFun' :: (CTuple f, Typeable r) => Bool -> (f w -> Exp w r) -> Maybe FunctionIdent -> [(Var, CType)] -> T FunctionIdent 
addFun' isClass f ident context = do 
  nf <- gets nextFun
  let newName = fromMaybe ("function_" ++ show nf) ident 
  modify (\s -> s { nextFun = nf + 1 } ) 
  modify (\s -> s { functions = Fun f newName (nub context) isClass False : functions s })
  return newName

-- | Returns an unqiue variable, tagged with the current function's name
newVar :: T Var
newVar = do
    var <- gets (head . nameSupply)
    modify (\t -> t { nameSupply = tail (nameSupply t) })
    cf <- gets currentFun 
    return $ Var var cf 

-- | Creates a unique variable, expect if the given type is "void", then
-- a special void value is given
newVarType :: CType -> T Var
newVarType x | x == "void" = return (Var (-1) Nothing)
newVarType _               = newVar

variable :: Var -> Exp w a
variable = Variable

-- | Translates a 'Exp' to the corresponding C++ code
-- The string denotes the name of the main function
translate :: Typeable a => String -> Exp w a -> Stmt
translate name (e::Exp w a) =  SList . removeDubFun 
                          $  translateFuncs nf fs
                          ++ [SFunc (toCType (undefined :: a)) 
                                     name [] (nicer stmt') False]
  where (v, stmt, fs,nf) = runT $ do
            (var, s) <- translateY e
            fncs <- gets functions
            nf <- gets nextFun
            return (var, s, fncs, nf)
        stmt' = case toCType (undefined :: a) of
            "void" -> stmt
            ret    -> SList [stmt, SReturn (var v)]

functionIdent (SFunc typ name _ _ _) = name ++ "@" ++ typ
functionIdent (SClass typ name _ _ _) = name ++ "@" ++ typ

-- | Removes functions which has the same body
removeDubFun xs = removeDubFun' xs []
  where 
    removeDubFun' [] _ = []
    removeDubFun' (x:xs) ys | functionIdent x `elem` ys = removeDubFun' xs ys
                            | otherwise = x : removeDubFun' xs (functionIdent x : ys)

-- | Translates an expression to a variable containing the result and the code
-- generated
translateY :: Exp w a -> T (Var, Stmt)
translateY expr = case expr of
    Prim a | toCType a == "void" -> return (VarBottom, SNop)
    Prim a -> do 
      var <- newVar 
      return (var, assignment True var (EValue (toCValue a) ::: toCType a))
    FMap  _ a   -> translateY a
    IterMap _ a -> translateY a
    Variable  a -> return (a,SNop)    
    Call fi (_ :: FunctionType (a -> HS w r) (b w -> HS w r)) inp -> do 
        var' <- newVar 
        (vars, stmts) <- tupleFold (translateY >=> \(a,b) -> return ([a],b)) inp
        return (var', SList $ stmts ++
           [assignment True var' (callCon fi (map var vars) ::: toCType (undefined :: r))])
      where 
        callCon :: FunctionKind -> [AST.Exp] -> AST.Exp
        callCon (FCall n) exps         = ECall [] n exps
        callCon (FInitClass c) exps    = ECall (namespace c) (classname c) exps
        callCon (FBinary n) (e1:e2:[]) = EBin n e1 e2
        callCon (FUnary n) (e1:[])     = EUnary n e1 
        --  the first element (EVar cp) corresponds to 'this' in C++ and should 
        --  not be used in the actual call
        callCon (FMethod n isPtr) (EVar cp :exps) = EMethod cp [] n exps isPtr
        callCon (FStatic c n) exps     = ECall (namespace c ++ [classname c]) n exps
    Let m f -> do
        (r,s)   <- translateY m
        (r',s') <- translateY (f (variable r))
        return (r', SList [s, s'])
    If p tru fal -> do
        (pvar, pstmt) <- translateY p
        (tvar, tsmt)  <- translateY tru
        (fvar, fsmt)  <- translateY fal
        let typ    = expToType tru
        let isVoid = typ == "void"
        result <- newVarType typ
        return (result, SList 
            [ decl result typ
            --, stmt
            , pstmt
            , SIf (var pvar) 
                (if isVoid
                    then tsmt
                    else SList [tsmt, assignment False result (var tvar ::: typ)]
                )
                (if isVoid
                    then fsmt
                    else SList [fsmt, assignment False result (var fvar ::: typ)]
                )
            ])
    While p body s -> do
         (svar, stmt)  <- translateY s
         let var' = variable svar
         (pvar, pstmt) <- translateY (p var')
         (pvar', pstmt') <- translateY (p var')
         (res, rstm)   <- translateY (body var')
         let typ = expToType (body var')
         return (svar,SList [stmt
                             , pstmt
                             , SWhile (var pvar) (SList 
                                                [ rstm
                                                , assignment False svar 
                                                    (var res ::: typ )
                                                , pstmt'
                                                , assignment False pvar 
                                                    (var pvar' ::: toCType True)
                                                ])
                             ])
    ForAll ds (f::(Exp w a -> Exp w b)) ->
        case toCType (undefined :: b) of
            "void" -> error "ForAll resulting in list of void!!1"
            _ -> do
                (derefVar, whileMaker) <- makeIteration ds
                (newList, newListStmt) <- -- Transformed list
                    translateY $ GP.list ([] :: [Exp w b])
                (transformVar, transformElem) <-
                      translateY $ f $ Variable derefVar
                let newElemAssign = SExpr $ method newList [] "push_back"
                      [var transformVar] False ::: "forall bug" 
                let loop = whileMaker $ SList [transformElem, newElemAssign]
                return (newList, SList [ SComment "### MAP START ###"
                                         , newListStmt
                                         , loop
                                         , SComment "### MAP END   ###"
                                         ])
    ManipulateList _ (_::Exp w xs) -> do
        let typ = toCType (undefined :: xs)
        (xsVar, ims, preconStmt) <- separateIterators expr
        (lengthVar, lengthStmt) <- translateY $ GP.length (Variable xsVar :: Exp w xs)
        let (dir, beginManip, endManip) = ti (Variable lengthVar) ims
        (bManVar, bManPrecon) <- translateY beginManip
        (eManVar, eManPrecon) <- translateY (endManip + 1)
        [begin, end] <- replicateM 2 newVar
        let (bExp, eExp) = case dir of
                Nom -> getIterators $ var xsVar ::: typ
                Rev -> getReverseIterators $ var xsVar ::: typ
        let bStmt = assignment True begin bExp
        let eStmt = assignment True end   eExp
        let bAdv = SExpr $ ECall ["std"] "advance" [var begin, var bManVar] ::: toCType ()
        let eAdv = SExpr $ ECall ["std"] "advance" [var end  , var eManVar] ::: toCType ()
        newLsVar <- newVar
        let newLsDecl = decl newLsVar typ
        let os = getInsertIterator (var newLsVar ::: typ)
        let newLsStmt = SExpr $ ECall ["std"] "copy" [var begin, var end, unTExp os] ::: typ
        return (newLsVar, SList [preconStmt,lengthStmt,bStmt,bManPrecon,bAdv,eStmt,eManPrecon,eAdv,newLsDecl,newLsStmt])
    UseList xs -> translateY xs
    UseIterable cls -> translateY cls
    
    WrapFunction f ident inp' -> do 
      (inp, inp_stmts) <- tupleTrans inp' (translateY >=> \(a,b) -> return ([a],b))
      let stmt = f inp 
      vars <- return . fst =<< getTupleVars inp
      let contextVars = nub $ filter (\(a,_) -> a `notElem` vars) $ freeVars (f (bottomTuple inp))
      let table = zip (map fst contextVars) [0..]
      let contextVariable = map (\(a,v) -> var a) contextVars
      newName <- addFun f ident contextVars 
      (var, SList stmts) <- translateY (Call (FCall newName) (PureH $ exp2hs f) inp)
      return (var, SList (SComment ("WF Vars: " ++ show vars) : inp_stmts ++ init stmts ++ [addContext contextVariable (last stmts)]))
     where
       exp2hs :: (CTuple f, Typeable r) => (f w -> Exp w r) -> f w -> HS w r 
       exp2hs f = \x -> DoExp (f x)

       addContext ctx (SAssignment v b (ECall id nm xs ::: typ)) = 
                       SAssignment v b (ECall id nm ((ctx \\\ xs) ++ xs) ::: typ)
       pos (EVar x) | x < -1    = EVar $ -2 - x
                    | otherwise = EVar x

       [] \\\ _        = []
       (x:xs) \\\ ys | x `elem` ys  = xs \\\ ys
                     | otherwise    = x : xs \\\ ys

    FunctionPtr (f :: f w -> Exp w r ) -> do 
      inp <- mkTuple (error "translateFunc: mkTuple" :: f w) (return  VarBottom) 
      let stmt = f inp
      let contextVars = nub $ filter (\(v,_) -> v /= VarBottom) (freeVars stmt)
      newName <- if null contextVars 
             then addFun f Nothing contextVars
             else addPartial f Nothing contextVars
      var' <- newVar
      let types = intercalate "," (tupleTypes (undefined :: f w) toCType)
      if null contextVars 
        then return (var', SList [
         functionPtr var' (tupleTypes (undefined :: f w) toCType) 
                          (toCType (undefined :: r))
                          (EValue newName)])

        
        else return (var', SList [
         assignment True var' (ECall [] newName (map (var . fst) contextVars) ::: newName) ])
    x -> error $ "translateY: " ++ inspect x

-- | Returns free variables 
freeVars :: Typeable a => Exp w a -> [(Var, CType)]
freeVars = filter ((VarBottom /=) . fst) . freeVars'
  where 
    freeVars' (Variable i :: Exp w a ) = [(i, toCType (undefined :: a))]
    freeVars' (Call a _ inp)   = tupleVars inp 
    freeVars' (If a tru fls) = freeVars a ++ freeVars tru ++ freeVars fls
    freeVars' (Prim _)   = []
    freeVars' (Let a b) = freeVars (b a)
    freeVars' (UseList xs) = freeVars xs
    freeVars' (UseIterable cls) = freeVars cls
    freeVars' (WrapFunction f _ inp) = freeVars (f $ bottomTuple inp) ++  tupleVars inp 
    freeVars' (FunctionPtr f )     = freeVars (f $ bottomTuple undefined) 
    freeVars' (FMap _  x) = freeVars x
    freeVars' (IterMap _ x) = freeVars x
    freeVars' (While p f inp) = freeVars (p inp) ++ freeVars (f inp)
    freeVars' (ManipulateList manip xs) = freeVars xs ++ case manip of 
        Reverse               -> []
        AdvanceBegin        n -> freeVars n
        DeclineEnd          n -> freeVars n
        AdvanceEndFromBegin n -> freeVars n
        DeclineBeginFromEnd n -> freeVars n
    freeVars' x = error ("freeVars: " ++ inspect x)

-- | create a tuple of only _|_
bottomTuple :: CTuple t => t w -> t w
bottomTuple (_ :: t w ) = runT $ mkTuple (undefined :: t w) (return VarBottom)

tupleVars inp = concatMap fst $ tupleFold (\e -> return (freeVars e, error "you don't need me")) inp 

freeVarsManip Reverse                 = []
freeVarsManip (AdvanceBegin        n) = freeVars n
freeVarsManip (DeclineEnd          n) = freeVars n
freeVarsManip (AdvanceEndFromBegin n) = freeVars n
freeVarsManip (DeclineBeginFromEnd n) = freeVars n

replaceFV :: FunctionIdent -> Int -> Exp w a -> Exp w a
replaceFV fi offset stmt = case stmt of
    Variable (Var i (Just id)) 
                               | id == fi  -> stmt
                               | otherwise -> Variable $ Var (i + offset) (Just fi)
    Variable _              -> stmt
    Prim _                  -> stmt 
    FunctionPtr _           -> stmt
    Call a b inp            -> Call a b 
                                (replace (\_ _ -> replaceFV fi offset) [] inp)
    If a tru fls            -> If (replaceFV fi offset a) 
                                  (replaceFV fi offset tru)
                                  (replaceFV fi offset fls)
    UseList xs              -> UseList (replaceFV fi offset xs)
    UseIterable cls         -> UseIterable (replaceFV fi offset cls)
    Let a b                 -> Let (replaceFV fi offset a)
                                   (replaceFV fi offset . b)


    WrapFunction a b inp    -> WrapFunction a b 
                                (replace (\_ _ -> replaceFV fi offset) [] inp)

    FMap f x                -> FMap f $ replaceFV fi offset x
    IterMap f x             -> IterMap f $ replaceFV fi offset x
    ManipulateList manip xs -> ManipulateList (replaceFVManip' fi offset manip)
                                              (replaceFV fi offset xs)
    While p f inp           -> While (replaceFV fi offset . p)
                                     (replaceFV fi offset . f) 
                                     (replaceFV fi offset inp)
    _                       -> error $ "replaceFV:" ++ inspect stmt 





magicInp inp = runT $ return . fst =<< getTupleVars inp

replaceFVManip' :: FunctionIdent -> Int-> IteratorManipulator w -> IteratorManipulator w
replaceFVManip' fi offset manip = case manip of
    AdvanceBegin        n -> AdvanceBegin        $ rep n
    DeclineEnd          n -> DeclineEnd          $ rep n
    AdvanceEndFromBegin n -> AdvanceEndFromBegin $ rep n
    DeclineBeginFromEnd n -> DeclineBeginFromEnd $ rep n
    m -> m
  where rep = replaceFV fi offset



remove (a@(x,_):xs) i | x `elem` i = remove xs i
                    | otherwise  = a : remove xs i
remove [] _ = []

toVar (Variable i) = [i]
toVar _  = []


separateIterators :: (Iterable xs, Typeable (Elem xs)) => Exp w xs -> T (Var, [IteratorManipulator w], Stmt)
separateIterators (xs :: Exp w ys)= do
    (v,s,ims) <- sep xs []
    return (v, ims, s)
  where sep :: Exp w ys -> [IteratorManipulator w] -> T (Var, Stmt, [IteratorManipulator w])
        sep (ManipulateList im xs) ims = sep xs (im:ims)
        sep xs ims                     = do
                        (xsVar,s) <- translateY xs
                        return (xsVar, s, ims)

data Direction = Nom | Rev
type IterStatus w = (Direction, Exp w Int, Exp w Int)

turn :: Direction -> Direction
turn Nom = Rev
turn _   = Nom

ti :: Exp w Int -> [IteratorManipulator w] -> IterStatus w
ti length = foldl (ti' length) (Nom, Prim 0, length)

ti' :: Exp w Int -> IterStatus w -> IteratorManipulator w -> IterStatus w
ti' length (d, begin, end) im = case im of
    Reverse               -> (turn d, length - end             , length - begin)
    AdvanceBegin        n -> (d     , GP.min (begin GP.+ n) end, end)
    DeclineEnd          n -> (d     , begin                    , GP.max begin (end GP.- n))
    AdvanceEndFromBegin n -> (d     , begin                    , GP.min (begin GP.+ n) end)
    DeclineBeginFromEnd n -> (d     , GP.max begin (end GP.- n), end)
    
    
-- | makeIteration takes a list and gives back the tuple of
--   a variable and a function that fills the loop body.
--
--   [@derefVar@]The returned variable that contains the value of the list
--   element inside the loop (to be used in statements).
--
--   [@whileMaker@]The returned function that takes the statements to be used
--   within the loop and inserts them at the appropriate place. Returned
--   is a 'Stmt' containing all loop-specific preconditions and the loop
--   itself.
--

assignment v (toAVar -> var) = SAssignment v var
deref (toAVar -> var) = EDeref var 
var = EVar . toAVar 
decl (toAVar -> var) = SDecl var
method (toAVar -> var) = EMethod var 
functionPtr (toAVar -> var) = SFunctionPtr var 

makeIteration :: (Typeable (Elem xs), Iterable xs) => Exp w xs -> T (Var, Stmt -> Stmt)
makeIteration (ds::Exp w xs) = do
    (list, listStmt) <- translateY ds
    derefVar     <- newVar -- Dereferenced element after transformation
    let elemType = toCType (undefined :: Elem xs)
    let newElemDecl = decl derefVar elemType
    let typedList = var list ::: toCType (undefined :: xs)
    bVar <- newVar
    eVar <- newVar
    let (begCall, endCall) = getIterators typedList
    let begin = assignment True bVar begCall
    let end   = assignment True eVar endCall
    let cond    = EBin "!=" (var bVar) (var eVar)
    let getElem = assignment False derefVar (deref bVar ::: elemType)
    let inc     = SExpr $ ECall ["std"] "advance" [var bVar, EValue (toCValue (1::Int))] ::: ""
    let whileMaker adaptations = SList  [ SComment "## ITERATION START ##"
                                        , listStmt
                                        , newElemDecl
                                        , begin
                                        , end
                                        , SWhile cond (SList [ getElem
                                                             , adaptations
                                                             , inc
                                                             ])
                                        , SComment "## ITERATION END   ##"
                                        ]
    return (derefVar, whileMaker)


-- | Input:  Typed EVar
--   Output: (v.begin(), v.end())
getIterators, getReverseIterators :: TExp -> (TExp, TExp)
getIterators c = (begin, end)
    where begin = getIterator c "iterator" "begin"
          end   = getIterator c "iterator" "end"
getReverseIterators c = (begin, end)
    where begin = getIterator c "reverse_iterator" "rbegin"
          end   = getIterator c "reverse_iterator" "rend"
          
getIterator :: TExp -> CType -> String -> TExp
getIterator (EVar classe ::: typ) t n  = EMethod classe [] n [] False ::: (typ ++ "::" ++ t)

getInsertIterator :: TExp -> TExp
getInsertIterator lstyped@(EVar ls ::: typ) = ECall ["std"] ("insert_iterator<" ++ typ ++ " >") [EVar ls, unTExp it] ::: itFullType
    where itTyp = "iterator"
          it@(_ ::: itFullType) = getIterator lstyped itTyp "begin"


hsToType :: Typeable a => HS w a -> CType
hsToType (_ :: HS w a) = toCType (undefined :: a)

expToType :: Typeable a => Exp w a -> CType
expToType (_ :: Exp w a) = toCType (undefined :: a)

-- | nicer does several things
-- * Collapses SList (x : SLit ys : xs) to SList (x ++ ys ++ xs)
-- * Removes SNops from SLists
-- * Changes assignments of type "void" to statments 
nicer :: Stmt -> Stmt
nicer stm = nicer' (voids stm) stm
nicer' :: [AST.Var] -> Stmt -> Stmt
nicer' table stm = case stm of
    SFunc ret nm vars s x -> SFunc ret nm vars (nicer s) x
    SList [] -> SNop
    SList x  -> SList $ help table x --[ s | s <- help x, notSNop s ]
    SAssignment _ y (EVar y' ::: x) | y == -1 && y' == -1 -> SNop
    SAssignment True _ (exp ::: x) | x == "void" -> nicer' table $ SExpr (exp ::: x)
    SDecl _ x | x == "void" -> SNop 
    SExpr (EValue x ::: _) | x == "void" -> SNop
    SIf e s1 s2  -> SIf e (nicer' table s1) (nicer' table s2)
    SWhile e s -> SWhile  e (nicer' table s)
    SReturn (EVar y') | y' `elem` table -> SNop
    x -> x
  where
    help tbl []              = []
    help tbl (SList xs : ys) = help tbl xs ++ help tbl ys
    help tbl (x : ys)        = nicer' tbl x : help tbl ys
    notSNop SNop = False
    notSNop s    = True

-- | Returns all variable pointing to void values
voids :: Stmt -> [AST.Var]
voids stm = -1 : case stm of
    SList x  -> concatMap voids x
    SAssignment True v (exp ::: x) | x == "void" -> [v]
    SDecl v x | x == "void" -> [v]
    SIf e s1 s2  -> voids s1 ++ voids s2
    SWhile e s -> voids s 
    _ -> [] 

-- | Translates a function into code, the Integer given corresponds to the
-- function name and the integer return to the next avaible name 
-- [Fun] returned should be positioned before the generated code
translateFunc :: Int -> Fun -> (Int, [Fun], Stmt)
translateFunc i (Fun (f :: t w -> Exp w r) ident ctx isClass isConst) = 
 let (v, state) = runTS i it 
  in (nextFun state, functions state, nicer v)
  where 
  err :: a 
  err = error "translateFunc"
  it = do 
    modify (\s -> s { nextFun = nextFun s + 1 })
    modify (\s -> s { currentFun = Just ident })
    let args = tupleTypes (err :: t w) toCType
    let ret  = toCType (err :: r)
    input <- mkTuple (err :: t w) newVar 
    inputMax <- (return . toAVar) =<< newVar

    let stmt = f input 
    let stmt' = replaceFV ident inputMax stmt 
    let context = map (swap . (\(Var a _,b) -> (a + inputMax,b))) ctx 
    replicateM_ (maximum (0 : map snd context) + 1) newVar 
    (res, body) <- translateY stmt'
    let body' = if toCType (undefined :: r) /= "void"
                then SList [body, SReturn (var res)]
                else body
    if isClass 
      then return $ SClass ret ident context
                                        (zip args [0..]) body'
      else return $ SFunc ret ident (zip args [0..] ++ context) body' isConst


getTupleVars inp = tupleFold helpFold inp
  where 
    helpFold :: Exp w a -> T ([Var],Var)
    helpFold exp = case exp of 
        Variable i -> return ([i],Var 0 Nothing)
        _          -> return ([],Var 0 Nothing)

