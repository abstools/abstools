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
import Control.Monad.Coroutine (mapMonad)
import Control.Monad.Coroutine.SuspensionFunctors (yield)

-- this :: (Object_ o) => ABS o (ObjectRef o)
-- this = do
--   t <- liftM aThis $ lift RWS.ask
--   return t

thisCOG :: (Object_ o) => ABS o COG
thisCOG = do
  t <- liftM aCOG $ lift RWS.ask
  return t

skip :: (Object_ o) => ABS o ()
skip = return (())

suspend :: ABS o ()
suspend = yield S

  -- (AConf obj chan _ f) <- RWS.ask
  -- lift $ writeChan chan (RunJob obj f cont) 
  -- return (())

sync_call :: (Object_ o, Object_ a) => ObjectRef a -> (ObjectRef a -> ABS a b) -> ABS o b
sync_call obj@(ObjectRef ioref _) call = do
  hereCOG <- thisCOG
  obj1 <- lift $ lift $ readIORef ioref
  otherCOG <- whereis obj1
  when (hereCOG /= otherCOG) $ error "Sync Call on a different COG detected"
  mapMonad (withReaderT (\ aconf -> aconf {aThis = obj})) (call obj)


await ::  (Object_ o) => AwaitGuard o -> ABS o () 
await (FutureGuard f@(FutureRef mvar _ _ _))  = do
  empty <- lift $ lift $ isEmptyMVar mvar
  when empty $ do
    yield (F f)

await (FutureGuard f@(FutureRef mvar _ _ _) :&: gs)  = do
  empty <- lift $ lift $ isEmptyMVar mvar
  when empty $ do
    yield (F f)
  await gs


await g@(ThisGuard is tg) = do
  check <- tg
  when (not check) $ do
       AConf obj _ _ <- lift $ RWS.ask
       yield (T obj is)
       await g

await gs@(ThisGuard is tg :&: rest) = do
  check <- tg
  when (not check) $ do
       AConf obj _ _ <- lift $ RWS.ask
       yield (T obj is)
       await gs
  await rest

async_call :: (Object_ o, Object_ a) => ObjectRef a -> (ObjectRef a -> ABS a b) -> ABS o (FutureRef b)
async_call obj@(ObjectRef ioref _) call = do
  obj1 <- lift $ lift $ readIORef ioref
  loc <-  whereis obj1
  mvar <- lift $ lift $ newEmptyMVar -- The new future created
  AConf {aThread = tid, aCOG = cog} <- lift $ RWS.ask
  astate@(AState {aCounter = counter}) <- lift $ RWS.get
  lift $ RWS.put (astate {aCounter = counter + 1})
  let f = FutureRef mvar tid cog counter
  lift $ lift $ writeChan loc (RunJob obj f (call obj))
  return f

while :: ABS o Bool -> ABS o () -> ABS o ()
while pred action = do
  res <- pred
  when res $ do
      action
      while pred action

get :: (Object_ o) => FutureRef f -> ABS o f
get a = (\ (FutureRef mvar _ _ _) -> lift $ lift $ readMVar mvar) a

readObject :: (Object_ o) => ObjectRef f -> ABS o f
readObject (ObjectRef ioref _) = lift $ lift $ readIORef ioref

null :: ObjectRef a
null = undefined
