{-# LANGUAGE Rank2Types, NoImplicitPrelude, ImpredicativeTypes,
  LiberalTypeSynonyms #-}
module Functions where
import qualified Control.Monad.Trans.RWS as RWS
import ABSPrelude
 
getLeft :: Either a b -> a
getLeft e = left e
 
f :: Int -> Int
f y = (\ x -> (\ x -> x + 1) (x + 4)) (y + 3)
 
either :: Either a a -> a
either e
  = case e of
        Left l -> l
        Right r -> r