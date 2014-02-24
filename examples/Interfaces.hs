{-# LANGUAGE Rank2Types, NoImplicitPrelude, ImpredicativeTypes,
  LiberalTypeSynonyms #-}
module Interfaces where
import qualified Control.Monad.Trans.RWS as RWS
import ABSPrelude
 
class (Object_ a) => Interf1_ a where
         
        method1 :: Int -> Int -> ObjectRef a -> ABS a Int
 
type Interf1 = forall a . (Interf1_ a) => ObjectRef a
 
class (Interf1_ a) => Interf2_ a where
         
        method2 :: Bool -> ObjectRef a -> ABS a ()
         
        method3 :: Int -> Interf1 -> ObjectRef a -> ABS a Bool
 
type Interf2 = forall a . (Interf2_ a) => ObjectRef a