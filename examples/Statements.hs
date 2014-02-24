{-# LANGUAGE Rank2Types, NoImplicitPrelude, ImpredicativeTypes,
  LiberalTypeSynonyms #-}
module Statements where
import qualified Control.Monad.Trans.RWS as RWS
import ABSPrelude
mainABS
  = do o1 <- new (class1 1 2 3)
       o1 `sync_call` method1 1 3
       o1 `sync_call` method2 True
       f <- o1 `async_call` method1 1 4
       await (FutureGuard f)
       f2 <- o1 `async_call` method2 True
       await (FutureGuard f2)
       res <- get (return f)
       res_ <- o1 `sync_call` method3 res
       return ()
main = main_is mainABS