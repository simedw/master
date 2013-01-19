{-# LANGUAGE FlexibleContexts, GADTs #-}

module Generator.Compiler where
import Generator.Compiler.Translator
import Generator.Interpreter
import Generator.Compiler.AST hiding (Exp)
import Generator.Expr 
import Generator.Internal.Expr (Exp(Variable,Let), Var(..))
import Generator.Internal.Optimize
import Generator.Compiler.Pretty
import Generator.Types
import Generator.Internal.Types
import Generator.Compiler.Optimize
--import Rule.Rule

import System.Process
import System.Exit

type SourceCode = String

defaultName = "main"

compileNRun :: Typeable a => Exp w a -> IO ()
compileNRun exp = compileEval compileToFile exp >>= putStrLn


--compileEval:: Typeable a => Exp w a -> IO String
compileEval f exp = do
    f "compilenrun.cpp" "main" exp
    (exitCode, stdout, stderr) <- readProcessWithExitCode "CC" ["compilenrun.cpp", "-o", "compilenrun"] ""
    case exitCode of
        ExitSuccess -> do 
            (ec, out, err) <- readProcessWithExitCode "./compilenrun" [] ""
            case ec of
                ExitSuccess -> return out
                n -> error "C++ runtime error"
        n -> error "C++ compiletime error"

compileNRunE :: Typeable a => Exp w a -> IO ()
compileNRunE = compileNRun . optimizeExp

compile,compileOS,compileOE,compileOSE :: Typeable a => String -> Exp w a -> SourceCode
compile    name = boilerplate . prettyPrint . translate name
compileOS  name = boilerplate . prettyPrint . optimize . translate name
compileOE  name = compile   name . optimizeExp
compileOSE name = compileOS name . optimizeExp

compileP, compilePOS, compilePOE, compilePOSE  :: Typeable a => Exp w a -> IO ()
compileP    = putStr . compile   defaultName . (`Let` const (0 :: Exp w Int))
compilePOS  = putStr . compileOS defaultName . (`Let` const (0 :: Exp w Int))
compilePOE  = compileP   . optimizeExp
compilePOSE = compilePOS . optimizeExp

compileToFile, compileToFileOS, compileToFileOE, compileToFileOSE :: Typeable a => FilePath -> String -> Exp w a -> IO()
compileToFile    path name = writeFile path . compile    name
compileToFileOS  path name = writeFile path . compileOS  name
compileToFileOE  path name = writeFile path . compileOE  name
compileToFileOSE path name = writeFile path . compileOSE name
{- rules 
compileR :: (Ruler rf, DataFunction (ParamRule rf), CFun (Exposed (ParamRule rf)))
                                                        => String -> rf -> SourceCode
compileR name = compileF . (Stop <|>) . (\f -> mkFunction name f undefined) . functionalize
compilePR :: (Ruler rf, DataFunction (ParamRule rf), CFun (Exposed (ParamRule rf)))
                                                        => String -> rf -> IO()
compilePR n = putStr . compileR n
compileToFileR :: (Ruler rf, DataFunction (ParamRule rf), CFun (Exposed (ParamRule rf)))
                                                        => FilePath -> String -> rf -> IO()
compileToFileR path name = writeFile path . compileR name
-}

compileF :: [Fun] -> SourceCode
compileF = boilerplate . prettyPrint . SList . translateFuncs 0
compilePF :: [Fun] -> IO()
compilePF = putStr . compileF
compileToFileF :: FilePath -> [Fun] -> IO()
compileToFileF path = writeFile path . compileF

sameAs :: Typeable a => Exp w a -> Exp w a -> Bool
e1 `sameAs` e2 = compile "sameAs_check" e1 == compile "sameAs_check" e2

optimizeExp :: Typeable a => Exp w a -> Exp w a
optimizeExp exp = snd $ until (\(old,new) -> old `sameAs` new) (\(old,new) -> (new, optPassExp new)) (Variable (Var 0 Nothing), exp)


boilerplate code = unlines $ 
    ["#include <iostream>"
    ,"#include <iterator>"
    ,"#include <list>"
    ,"#include <cstdarg>"
    ,"#include <algorithm>"
--    ,"#include <InterlockingState.hpp>"
--    ,"#include <gpu3.hpp>"
--   ,"#include \"code.hpp\""
    ,""
    ,"template<typename T>"
    ,"void print(T i) {"
    ,"    std::cout << i << std::endl;"
    ,"}"
    ,"template<typename T1, typename T2>"
    ,"T1 get21(std::pair<T1,T2> t) {"
    ,"    return t.first;"
    ,"}"
    ,"template<typename T1, typename T2>"
    ,"T2 get22(std::pair<T1,T2> t) {"
    ,"    return t.second;"
    ,"}"
    ,"template<typename T>"
    ,"std::list<T> mk_list(int amount, ...)"
    ,"{"
    ,"    std::list<T> l;"
    ,"    va_list argPtr;"
    ,"    va_start(argPtr, amount);"
    ,"    for (int i = 0; i < amount; i++)"
    ,"        l.push_back(va_arg(argPtr,T));"
    ,"    va_end(argPtr);"
    ,"    return l;"
    ,"}"
    ,"template<typename T, class Predicate>"
    ,"std::list<T> filter(std::list<T> list, Predicate pre)"
    ,"{"
    , "std::list<T> out;"
    , "std::insert_iterator< std::list<T> > insert_it(out,out.begin());"
    , "std::remove_copy_if(list.begin(),list.end(),insert_it,pre);"
    , "return out;"
    ,"}"
    ,"template<typename T, class UnaryOperator>"
    ,"std::list<T> map(std::list<T> list, UnaryOperator op)"
    ,"{"
    , "std::list<T> out;"
    , "std::insert_iterator< std::list<T> > insert_it(out,out.begin());"
    , "std::transform(list.begin(),list.end(),insert_it,op);"
    , "return out;"
    ,"}"
    , code
    ]

