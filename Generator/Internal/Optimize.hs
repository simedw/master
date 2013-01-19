{-# LANGUAGE GADTs #-}

module Generator.Internal.Optimize where

import Generator.Internal.Expr
import Generator.Types

fuseandsort :: Exp w a -> Exp w a
fuseandsort exp = case exp of
  --OPTIMIZING
    ManipulateList im (ForAll xs f) -> ForAll (ManipulateList im xs) f
    ForAll (ForAll xs f) g -> ForAll (fuseandsort xs) h
      where h a = fuseandsort (g . f $ a)
  --TRAVERSING
    Let a f    -> Let (fuseandsort a) g
      where g a = fuseandsort (f a)
    If b thn els -> If (fuseandsort b) (fuseandsort thn) (fuseandsort els)
    While cond body inp -> While cond' body' (fuseandsort inp)
      where cond' a = fuseandsort (cond a)
            body' a = fuseandsort (body a)
    --List xs -> List (map fuseandsort xs)
    ManipulateList im xs -> ManipulateList im (fuseandsort xs)
    ForAll xs f -> ForAll (fuseandsort xs) g
      where g a = fuseandsort (f a)
    --FoldL f init xs -> FoldL g (fuseandsort init) (fuseandsort xs)
    --  where g a b = fuseandsort (f a b)
    --New f a -> New f (fuseandsort a)
    Call ident f a -> Call ident f (tupleMap fuseandsort a)
    --DeRef ptr -> DeRef (fuseandsort ptr)
    --Cons x xs -> Cons (fuseandsort x) (fuseandsort xs)
    --MkList cls f -> MkList (fuseandsort cls) f
    UseList xs -> UseList (fuseandsort xs)
    UseIterable cls -> UseIterable (fuseandsort cls)
    x -> x

optPassExp :: Exp w a -> Exp w a
optPassExp = fuseandsort
