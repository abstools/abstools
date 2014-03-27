module Core where

import Base
import Data.List (foldl')
import Control.Monad (when)
import Control.Concurrent (ThreadId, myThreadId, forkIO)
import qualified Data.Map.Strict as M (empty, insertWith, updateLookupWithKey)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.Chan (newChan, readChan, writeChan, writeList2Chan, Chan)
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS as RWS (runRWST, execRWST, modify, RWST, withRWST)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield (..))
import System.Exit (exitSuccess)

spawnCOG :: Chan Job -> IO ThreadId
spawnCOG c = forkIO $ do
             tid <- myThreadId
             let sleepingF = M.empty :: FutureMap
             let sleepingO = M.empty :: ObjectMap
             -- the loop must be tail-recursive (not necessarily syntactically tail-recursive)
             -- to avoid stack leaks
             let loop sleepingF sleepingO counter = do
                        nextJob <- readChan c
                        case nextJob of
                          WakeupJob f -> do
                                       let (maybeWoken, sleepingF'') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture f) sleepingF
                                       maybe (return ()) (\ woken -> writeList2Chan c woken) maybeWoken -- put the woken back to the enabled queue
                                       loop sleepingF'' sleepingO counter
                          RunJob obj fut@(FutureRef mvar (fcog, ftid) _) coroutine -> do
                          (sleepingF'', (AState {aCounter = counter'', aSleepingO = sleepingO''}), _) <- RWS.runRWST (do
                                       p <- resume coroutine
                                       case p of
                                         Right fin -> do
                                                lift $ putMVar mvar fin
                                                when (ftid /= tid) (lift $ writeChan fcog (WakeupJob fut)) -- remote job finished, wakeup the remote cog
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
                                               aCOG = (c, tid)
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
  -- the loop must be tail-recursive (not necessarily syntactically tail-recursive)
  -- to avoid stack leaks
  let loop sleepingF sleepingO counter = do
           nextJob <- readChan c
           case nextJob of
             WakeupJob f -> do
                          let (maybeWoken, sleepingF'') = M.updateLookupWithKey (\ _k _v -> Nothing) (AnyFuture f) sleepingF
                          maybe (return ()) (\ woken -> writeList2Chan c woken) maybeWoken -- put the woken back to the enabled queue
                          loop sleepingF'' sleepingO counter
             RunJob obj fut coroutine -> do
               (sleepingF'', (AState {aCounter = counter'', aSleepingO = sleepingO''}), _) <- RWS.runRWST (do
                 p <- resume coroutine
                 case p of
                   Right fin -> case fut of
                                 (FutureRef mvar (fcog, ftid) _) -> do
                                          lift $ putMVar mvar fin
                                          when (ftid /= tid) (lift $ writeChan fcog (WakeupJob fut)) -- remote job finished, wakeup the remote cog
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
                             aCOG = (c, tid)
                            }) (AState {aCounter = counter,
                                        aSleepingO = sleepingO})
               loop sleepingF'' sleepingO'' counter''
  loop sleepingF sleepingO 1


-- util function, used in code generation
withReaderT :: (r' -> r) -> RWS.RWST r w s m a -> RWS.RWST r' w s m a
withReaderT f r = RWS.withRWST (\ r s -> (f r, s)) r
