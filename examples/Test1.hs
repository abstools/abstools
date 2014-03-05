{-# LANGUAGE Rank2Types, NoImplicitPrelude,
  ExistentialQuantification, MultiParamTypeClasses #-}
module Main where
import qualified Control.Monad.Trans.RWS as RWS
import Prim
import ABSPrelude
 
class (Object__ a) => Interf1_ a where
         
        method1 :: Int -> Int -> Interf1 -> ABS a Int
 
data Interf1 = forall a . (Interf1_ a) => Interf1 (ObjectRef a)
 
instance Sub Interf1 Interf1 where
        up x = x
 
instance Sub Interf1 AnyObject where
        up (Interf1 a) = AnyObject a
method1_sync n m (__wrapped@(Interf1 __obj@(ObjectRef __ioref _)))
  = do __hereCOG <- thisCOG
       __obj1 <- lift (lift (readIORef __ioref))
       otherCOG <- whereis __obj1
       when (__hereCOG /= otherCOG)
         (error "Sync Call on a different COG detected")
       mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
         (method1 n m __wrapped)
method1_async n m (__wrapped@(Interf1 __obj@(ObjectRef __ioref _)))
  = do __obj1 <- lift (lift (readIORef __ioref))
       __loc <- whereis __obj1
       __mvar <- lift (lift newEmptyMVar)
       AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
       astate@(AState{aCounter = __counter}) <- lift RWS.get
       lift (RWS.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __tid cog __counter
       lift
         (lift (writeChan __loc (RunJob __obj __f (method1 n m __wrapped))))
       return __f
 
data Class1 = Class1{class1_loc :: (Object__ o) => ABS o COG,
                     class1_p1 :: Int, class1_p2 :: Int, class1_p3 :: Int,
                     class1_x :: Int}
__class1 p1 p2 p3
  = Class1{class1_p1 = p1, class1_p2 = p2, class1_p3 = p3}
 
instance Object__ Class1 where
        new __cont
          = do __chan <- lift (lift newChan)
               let __x = 0
               let __c = __cont{class1_x = __x, class1_loc = return __chan}
               __ioref <- lift (lift (newIORef __c))
               let __obj = ObjectRef __ioref 0
               lift (lift (spawnCOG __chan))
               do __mvar <- lift (lift newEmptyMVar)
                  AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
                  astate@(AState{aCounter = __counter}) <- lift RWS.get
                  lift (RWS.put (astate{aCounter = __counter + 1}))
                  let __f = FutureRef __mvar __tid cog __counter
                  lift
                    (lift
                       (writeChan __chan (RunJob __obj __f (__init (AnyObject __obj)))))
               do __mvar <- lift (lift newEmptyMVar)
                  AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
                  astate@(AState{aCounter = __counter}) <- lift RWS.get
                  lift (RWS.put (astate{aCounter = __counter + 1}))
                  let __f = FutureRef __mvar __tid cog __counter
                  lift
                    (lift
                       (writeChan __chan (RunJob __obj __f (__run (AnyObject __obj)))))
               return __obj
        new_local __cont
          = do let __x = 0
               let __c = __cont{class1_x = __x, class1_loc = thisCOG}
               __ioref <- lift (lift (newIORef __c))
               __astate@(AState{aCounter = __counter}) <- lift RWS.get
               lift (RWS.put (__astate{aCounter = __counter + 1}))
               let __obj = ObjectRef __ioref __counter
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__init (AnyObject __obj))
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__run (AnyObject __obj))
               return __obj
        whereis = class1_loc
        __init this
          = do (set_class1_x =<< (return 1))
               (set_class1_x =<<
                  ((readThis >>= \ Class1{class1_x = __x} -> return (__x + 1))))
               return ()
 
set_class1_p1 :: Int -> ABS Class1 ()
set_class1_p1 v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{class1_p1 = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 0) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_class1_p2 :: Int -> ABS Class1 ()
set_class1_p2 v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{class1_p2 = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 1) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_class1_p3 :: Int -> ABS Class1 ()
set_class1_p3 v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{class1_p3 = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 2) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
set_class1_x :: Int -> ABS Class1 ()
set_class1_x v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{class1_x = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 3) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
instance Interf1_ Class1 where
        method1 n m this = do return (n + m)
mainABS
  = do o1 <- liftM Interf1 (new (__class1 1 2 3)) :: ABS Top Interf1
       o2 <- liftM Interf1 (new (__class1 3 4 5)) :: ABS Top Interf1
       f1 <- (method1_async 3 4 (up o1)) :: ABS Top (Fut Int)
       await (FutureGuard f1)
       res1 <- get (f1)
       assert (return (res1 == 7))
       f2 <- (method1_async 4 5 (up o2)) :: ABS Top (Fut Int)
       await (FutureGuard f2)
       res2 <- get (f2)
       assert (return (res2 == 9))
main = main_is mainABS