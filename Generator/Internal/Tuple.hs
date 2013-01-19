{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeFamilies  #-}
module Generator.Internal.Tuple 
 ( Tuple0(..)
 , Tuple1(..)
 , Tuple2(..)
 , Tuple3(..)
 , Tuple4(..)
 , Tuple5(..)
 , tupleUncurry1
 , tupleUncurry2
 , tupleUncurry3
 , tupleUncurry4
 , tupleUncurry5
 , tupleCurry1
 , tupleCurry2
 , tupleCurry3
 , tupleCurry4
 , tupleCurry5
 ) where

import Generator.Internal.Expr hiding (return, (>>=))
import Generator.Internal.Types

data Tuple0 k w           = T0
data Tuple1 k a w         = T1 { unT1 :: (k w a) }
data Tuple2 k a b w       = T2 { unT2 :: (k w a, k w b) }
data Tuple3 k a b c w     = T3 ( k w a, k w b, k w c )
data Tuple4 k a b c d w   = T4 ( k w a, k w b, k w c, k w d )
data Tuple5 k a b c d e w = T5 ( k w a, k w b, k w c, k w d, k w e )


tupleUncurry1 :: (k w a -> p w c) -> Tuple1 k a w -> p w c
tupleUncurry1 f (T1 (a)) = f a
tupleCurry1 :: (Tuple1 k a w -> p w c) -> k w a -> p w c
tupleCurry1 f a = f (T1 (a))

tupleUncurry2 :: (k w a -> k w b -> p w c) -> Tuple2 k a b w -> p w c
tupleUncurry2 f (T2 (a, b)) = f a b
tupleCurry2 :: (Tuple2 k a b w -> p w c) -> k w a -> k w b -> p w c
tupleCurry2 f a b = f (T2 (a,b))

tupleUncurry3 :: (k w a -> k w b -> k w c -> p w d) -> Tuple3 k a b c w -> p w d
tupleUncurry3 f (T3 (a,b,c)) = f a b c
tupleCurry3 :: (Tuple3 k a b c w -> p w d) -> k w a -> k w b -> k w c -> p w d
tupleCurry3 f a b c = f (T3 (a,b,c))

tupleUncurry4 :: (k w a -> k w b -> k w c -> k w d -> p w e) -> Tuple4 k a b c d w 
                                                             -> p w e
tupleUncurry4 f (T4 (a,b,c,d)) = f a b c d
tupleCurry4 :: (Tuple4 k a b c d w -> p w e) -> k w a -> k w b -> k w c -> k w d 
                                                                        -> p w e
tupleCurry4 f a b c d = f (T4 (a,b,c,d))

tupleUncurry5 :: (k w a -> k w b -> k w c -> k w d -> k w e -> p w f) 
                                          -> Tuple5 k a b c d e w -> p w f
tupleUncurry5 f (T5 (a,b,c,d,e)) = f a b c d e
tupleCurry5 :: (Tuple5 k a b c d e w -> p w f) -> k w a -> k w b -> k w c -> k w d 
                                                                 -> k w e -> p w f
tupleCurry5 f a b c d e = f (T5 (a,b,c,d,e))


instance CTuple (Tuple0 k) where
  type TInternal (Tuple0 k) = ()
  tupleRun run s _      = ((),s)
  tupleFold f _         = return ([],[])
  tupleMap _ _          = T0
  mkTuple _ _           = return T0
  tupleTypes _ _        = []
  back _                = T0
  replace _ _ _         = T0
  tupleSize _           = 0
  tupleTrans _ _        = return (T0,[])
 

instance (Typeable a) => CTuple (Tuple1 Exp a) where
  type TInternal (Tuple1 Exp a) = a
 
  tupleRun run s (T1 inp)    = run s inp 
  tupleFold f (T1 x) = f x >>= \(a,b) -> return (a,[b])
  tupleMap f (T1 x)  = T1 $ f x
  mkTuple _ g   = g >>= \r -> return $ T1 (Variable r)
  tupleTypes ((_ :: Tuple1 Exp x w))  f = [f (u :: x) ]
  back (a) = T1 $ Prim a
  replace replaceFV table (T1 a) = T1 $ replaceFV table [] a
  tupleSize _           = 1
  tupleTrans (T1 e1) f = do
    ([v1],s1) <- f e1
    return (T1 (Variable v1), [s1])
 
u = error "tuple undefined"

instance (Typeable a, Typeable b) => CTuple (Tuple2 Exp a b) where
  type TInternal (Tuple2 Exp a b) = (a, b)
  tupleRun run s (T2 (a,b))
                          = let (a',s') = run s a
                                (b',s'') = run s' b
                             in ((a',b'),s'')

  tupleFold f (T2 (x,y)) = f x >>= \(a,b) -> 
                      f y >>= \(c,d) -> return (a ++ c,b : [d])
  mkTuple _ g = g >>= \r -> 
                g >>= \r' ->
                  return $ T2 (Variable r, Variable r')
 
  tupleTypes ((_ :: Tuple2 Exp x y w))  f = [f (u :: x), f (u :: y) ]
  tupleMap f (T2 (x,y))  = T2 (f x, f y)
  back (a,b) = T2 (Prim a, Prim b)
  replace replaceFV table (T2 (a,b)) = T2 (replaceFV table [] a, replaceFV table [] b)
  tupleSize _           = 2
  tupleTrans (T2 (e1,e2)) f = do
    ([v1],s1) <- f e1
    ([v2],s2) <- f e2
    return (T2 (Variable v1, Variable v2), [s1,s2])

instance (Typeable a, Typeable b, Typeable c) => CTuple (Tuple3 Exp a b c) where
  type TInternal (Tuple3 Exp a b c) = (a, b, c)
  tupleRun run s (T3 (e1,e2,e3))
                          = let (v1,s1) = run s e1
                                (v2,s2) = run s1 e2
                                (v3,s3) = run s2 e3
                             in ((v1,v2,v3),s3)


  tupleFold f (T3 (x,y,z)) = f x >>= \(a,b) -> 
                      f y >>= \(c,d) -> 
                      f z >>= \(e,f) -> 
                      return (a ++ c ++ e,b : d : f : [])
  mkTuple _ g = g >>= \r -> 
                g >>= \r' ->
                g >>= \r'' ->
                  return $ T3 (Variable r, Variable r', Variable r'')
 
  tupleTypes ((_ :: Tuple3 Exp x y z w))  f = [f (u :: x), f (u :: y), f (u :: z) ]
  tupleMap f (T3 (x,y,z))  = T3 (f x, f y, f z)
  back (v1,v2,v3) = T3 (Prim v1, Prim v2, Prim v3)
  replace replaceFV table (T3 (a,b,c)) = T3 (replaceFV table [] a, replaceFV table [] b,  replaceFV table [] c)
  tupleSize _           = 3
  tupleTrans (T3 (e1,e2,e3)) f = do
    ([v1],s1) <- f e1
    ([v2],s2) <- f e2
    ([v3],s3) <- f e3
    return (T3 (Variable v1, Variable v2, Variable v3), [s1,s2,s3])
  

instance (Typeable a, Typeable b, Typeable c, Typeable d) => CTuple (Tuple4 Exp a b c d) where
  type TInternal (Tuple4 Exp a b c d) = (a, b, c, d)
  tupleRun run s (T4 (e1,e2,e3,e4))
                          = let (v1,s1) = run s e1
                                (v2,s2) = run s1 e2
                                (v3,s3) = run s2 e3
                                (v4,s4) = run s3 e4
                             in ((v1,v2,v3,v4),s4)

  tupleFold ff (T4 (x,y,z,w)) = ff x >>= \(a,b) -> 
                      ff y >>= \(c,d) -> 
                      ff z >>= \(e,f) -> 
                      ff w >>= \(g,h) -> 
                      return (a ++ c ++ e ++ g,b : d : f : h : [])
  mkTuple _ g = g >>= \r -> 
                g >>= \r' ->
                g >>= \r'' ->
                g >>= \r''' ->
                  return $ T4 (Variable r, Variable r', Variable r'', Variable r''')
 
  tupleTypes ((_ :: Tuple4 Exp x y z xx w))  f = [f (u :: x), f (u :: y), f (u :: z), f (u :: xx) ]
  tupleMap f (T4 (x,y,z, a))  = T4 (f x, f y, f z, f a)
  back (v1,v2,v3,v4) = T4 (Prim v1, Prim v2, Prim v3, Prim v4)
  replace replaceFV table (T4 (a,b,c,d)) = T4 (replaceFV table [] a, replaceFV table [] b,  replaceFV table [] c,  replaceFV table [] d)
  tupleSize _           = 4
  tupleTrans (T4 (e1,e2,e3,e4)) f = do
    ([v1],s1) <- f e1
    ([v2],s2) <- f e2
    ([v3],s3) <- f e3
    ([v4],s4) <- f e4
    return (T4 (Variable v1, Variable v2, Variable v3, Variable v4), [s1,s2,s3,s4])


instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => CTuple (Tuple5 Exp a b c d e) where
  type TInternal (Tuple5 Exp a b c d e) = (a, b, c, d, e)
  tupleRun run s (T5 (e1,e2,e3,e4,e5))
                          = let (v1,s1) = run s e1
                                (v2,s2) = run s1 e2
                                (v3,s3) = run s2 e3
                                (v4,s4) = run s3 e4
                                (v5,s5) = run s3 e5
                             in ((v1,v2,v3,v4,v5),s5)

  tupleFold ff (T5 (x,y,z,w,v1)) = ff x >>= \(a,b) -> 
                      ff y >>= \(c,d) -> 
                      ff z >>= \(e,f) -> 
                      ff w >>= \(g,h) -> 
                      ff v1 >>= \(i,j) ->
                      return (a ++ c ++ e ++ g ++ i,b : d : f : h : j : [])
  mkTuple _ g = g >>= \r -> 
                g >>= \r' ->
                g >>= \r'' ->
                g >>= \r''' ->
                g >>= \r'''' ->
                  return $ T5 (Variable r, Variable r', Variable r'', Variable r''', Variable r'''')
 
  tupleTypes ((_ :: Tuple5 Exp x y z xx yy w))  f = [f (u :: x), f (u :: y), f (u :: z), f (u :: xx), f (u :: yy)]
  tupleMap f (T5 (x,y,z,a,b))  = T5 (f x, f y, f z, f a, f b)
  back (v1,v2,v3,v4,v5) = T5 (Prim v1, Prim v2, Prim v3, Prim v4, Prim v5)
  replace replaceFV table (T5 (a,b,c,d,e)) = T5 (replaceFV table [] a, replaceFV table [] b,  replaceFV table [] c,  replaceFV table [] d, replaceFV table [] e)
  tupleSize _           = 5
  tupleTrans (T5 (e1,e2,e3,e4,e5)) f = do
    ([v1],s1) <- f e1
    ([v2],s2) <- f e2
    ([v3],s3) <- f e3
    ([v4],s4) <- f e4
    ([v5],s5) <- f e5
    return (T5 (Variable v1, Variable v2, Variable v3, Variable v4, Variable v5), [s1,s2,s3,s4,s5])

