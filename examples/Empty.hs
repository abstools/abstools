{-# LANGUAGE Rank2Types, NoImplicitPrelude, ImpredicativeTypes,
  LiberalTypeSynonyms #-}
module Empty where
import qualified Control.Monad.Trans.RWS as RWS
import ABSPrelude
mainABS = do assert (return (3 == 4))
main = main_is mainABS