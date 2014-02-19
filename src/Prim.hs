{-# LANGUAGE Rank2Types #-}

module Prim where

import Base
import Utils

import Control.Monad (liftM, when)
import Data.IORef (readIORef)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.RWS as RWS (RWST, ask, get, put)
import Control.Concurrent.MVar (newEmptyMVar, isEmptyMVar, readMVar)
import Control.Concurrent.Chan (writeChan)

this :: (Object_ o) => ABS o a (ObjectRef o)
this = do
  t <- liftM aThis RWS.ask
  return (R t)

thisCOG :: (Object_ o) => ABS o a COG
thisCOG = do
  t <- liftM aCOG RWS.ask
  return (R t)

skip :: (Object_ o) => ABS o a ()
skip = return (R ())

suspend cont = do
  (AConf obj chan _ f) <- RWS.ask
  lift $ writeChan chan (RunJob obj cont f) 
  return (R ())


sync_call obj@(ObjectRef ioref _) call = do
  R hereCOG <- thisCOG
  obj1 <- lift $ readIORef ioref
  R otherCOG <- whereis obj1
  when (hereCOG /= otherCOG) $ error "Sync Call on a different COG detected"
  withReaderT (\ aconf -> aconf {aThis = obj}) (call obj)


await :: forall a o.
           Object_ o =>
           AwaitGuard o a
           -> ABS o a a
           -> ABS o a a
await (FutureGuard f@(FutureRef mvar _ _)) cont  = do
  let check f  = do
        empty <- lift $ isEmptyMVar mvar
        if empty
          then do
          AConf obj _ _ fut  <- RWS.ask
          return (F f obj cont fut)
          else cont
  check f

await (FutureGuard f@(FutureRef mvar _ _) :&: gs) cont  = do
  empty <- lift $ isEmptyMVar mvar
  if empty
    then do
    AConf obj _ _ fut <- RWS.ask
    return (F f obj (await gs cont) fut)
    else await gs cont


await g@(ThisGuard is tg) cont = do
  R check <- tg
  if check
     then cont
     else do
       AConf obj _ _ fut <- RWS.ask
       return (T obj is (await g cont) fut)

await gs@(ThisGuard is tg :&: rest) cont = do
  R check <- tg
  if check
     then await rest cont
     else do
       AConf obj _ _ fut <- RWS.ask
       return (T obj is (await gs cont) fut)

async_call obj@(ObjectRef ioref _) call = do
  obj1 <- lift $ readIORef ioref
  R loc <- whereis obj1
  mvar <- lift $ newEmptyMVar -- The new future created
  AConf {aThread = tid} <- RWS.ask
  astate@(AState {aCounter = counter}) <- RWS.get
  RWS.put (astate {aCounter = counter + 1})
  let f = FutureRef mvar tid counter
  lift $ writeChan loc (RunJob obj (call obj) f)
  return (R f)

while :: ABS o a Bool -> ABS o a ()
while pred = do
  R res <- pred
  if (not res) 
    then while pred
    else return (R ())

get a = (\ (FutureRef mvar _ _) -> lift $ liftM R $ readMVar mvar) =<< a

readObject (ObjectRef ioref _) = readIORef ioref


