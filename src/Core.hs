module Core where

import Base
import Data.List (foldl')
import Control.Concurrent (ThreadId, myThreadId, forkIO)
import qualified Data.Map.Strict as M (empty, insertWith)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS as RWS (runRWST, execRWST, modify)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield (..))
import System.Exit (exitSuccess)

spawnCOG :: COG -> IO ThreadId
spawnCOG c = forkIO $ do
             tid <- myThreadId
             let sleepingF = M.empty :: FutureMap
             let sleepingO = M.empty :: ObjectMap
             let loop sleepingF sleepingO counter = do
                         RunJob obj fut@(FutureRef mvar _ _) coroutine <- readChan c
                         (sleepingF'', (AState {aCounter = counter'', aSleepingO = sleepingO''}), _) <- RWS.runRWST (do
                                       p <- resume coroutine
                                       case p of
                                         Right fin -> do
                                                lift $ putMVar mvar fin
                                                return sleepingF
                                         Left (Yield S cont) -> do
                                                lift $ writeChan c (RunJob obj fut cont) 
                                                return sleepingF
                                         Left (Yield (F f) cont) -> do
                                                return (M.insertWith (++) (AnyFuture f) [RunJob obj fut cont] sleepingF)
                                         Left (Yield (T o fields) cont) -> do
                                                let sleepingO' = foldl' (\ m i -> M.insertWith (++) ((AnyObject o,i)) [RunJob obj fut cont] m) sleepingO' fields
                                                RWS.modify $ \ astate -> astate {aSleepingO = sleepingO'}
                                                return sleepingF
                                     ) (AConf {aThis = obj, 
                                               aCOG = c, 
                                               aThread = tid
                                               })
                                       (AState {aCounter = counter,
                                                aSleepingO = sleepingO})
                         loop sleepingF'' sleepingO'' counter''
             loop sleepingF sleepingO 1

-- Haskell main thread
main_is :: ABS Top () -> IO () 
main_is mainABS = do
  tid <- myThreadId
  let sleepingF = M.empty :: FutureMap
  let sleepingO = M.empty :: ObjectMap
  c <- newChan
  writeChan c (RunJob (error "not this at top-level") TopRef mainABS)
  let loop sleepingF sleepingO counter = do
           RunJob obj fut coroutine <- readChan c
           (sleepingF'', (AState {aCounter = counter'', aSleepingO = sleepingO''}), _) <- RWS.runRWST (do
                 p <- resume coroutine
                 case p of
                   Right fin -> case fut of
                                 (FutureRef mvar _ _) -> do
                                          lift $ putMVar mvar fin
                                          return sleepingF
                                 TopRef -> do
                                          lift $ print "Main COG has exited with success"
                                          lift $ exitSuccess
                   Left (Yield S cont) -> do
                          lift $ writeChan c (RunJob obj fut cont) 
                          return sleepingF
                   Left (Yield (F f) cont) -> do
                          return (M.insertWith (++) (AnyFuture f) [RunJob obj fut cont] sleepingF)
                   Left (Yield (T o fields) cont) -> do
                          let sleepingO' = foldl' (\ m i -> M.insertWith (++) ((AnyObject o,i)) [RunJob obj fut cont] m) sleepingO' fields
                          RWS.modify $ \ astate -> astate {aSleepingO = sleepingO'}
                          return sleepingF
                   ) (AConf {aThis = obj, 
                             aCOG = c, 
                             aThread = tid
                            }) (AState {aCounter = counter,
                                        aSleepingO = sleepingO})
           loop sleepingF'' sleepingO'' counter''
  loop sleepingF sleepingO 1




