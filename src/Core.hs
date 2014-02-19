module Core where

import Base
import Data.List (foldl')
import Control.Concurrent (ThreadId, myThreadId, forkIO)
import qualified Data.Map.Strict as M (empty, insertWith)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.Chan (newChan, readChan)
import qualified Control.Monad.Trans.RWS as RWS (runRWST, execRWST)

spawnCOG :: COG -> IO ThreadId
spawnCOG c = forkIO $ do
             tid <- myThreadId
             let sleepingF = M.empty :: FutureMap
             let sleepingO = M.empty :: ObjectMap
             let loop sleepingF sleepingO counter = do
                         RunJob obj r f@(FutureRef mvar _ _) <- readChan c
                         (res, newState@(AState {aCounter = newCounter, aSleepingO = sleepingO'}), _) <- RWS.runRWST r (AConf {aThis = obj, 
                                                                                                                             aCOG = c, 
                                                                                                                             aThread = tid,
                                                                                                                             aFut = f})
                                                                                                       (AState {aCounter = counter,
                                                                                                                aSleepingO = sleepingO})
                         (sleepingF'', sleepingO'') <- case res of
                                                          R fin -> do
                                                                   putMVar mvar fin
                                                                   return (sleepingF, sleepingO')
                                                          F f obj cont fut -> do
                                                                   return (M.insertWith (++) (AnyFuture f) [RunJob obj cont fut] sleepingF, sleepingO')
                                                          T obj is cont fut -> do
                                                                 -- for each dependent object attr, update the object map
                                                                 let sleepingO'' = foldl' (\ m i -> M.insertWith (++) ((AnyObject obj,i)) [RunJob obj cont fut] m) sleepingO' is 
                                                                 return (sleepingF, sleepingO'')
                         loop sleepingF'' sleepingO'' newCounter
             loop sleepingF sleepingO 1

-- Haskell main
main_is :: ABS Top f () -> IO () 
main_is mainABS = do
  -- Main Cog
  topChan <- newChan
  spawnCOG topChan
  let topConf = AConf { 
                  aThis = error "not 'this' at top-level" , 
                  aCOG = topChan, 
                  aThread = error "the main thread should not be used for spawning futures or objects", 
                  aFut = error "not future at top-level"}
  let topState = AState {
                  aCounter = error "the main counter should not be used for spawning futures or objects",
                  aSleepingO = error "no sleeping jobs in main thread"
                }
  RWS.execRWST mainABS topConf topState
  return ()

