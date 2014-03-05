{-# LANGUAGE Rank2Types, NoImplicitPrelude,
  ExistentialQuantification, MultiParamTypeClasses #-}
module Main where
import qualified Control.Monad.Trans.RWS as RWS
import Prim
import ABSPrelude
getLeft e = left e
f y = (\ x -> (\ x -> (x + 1)) (x + 4)) (y + 3)
either e
  = case e of
        Left l -> l
        Right r -> r
mainABS = return ()
main = main_is mainABS