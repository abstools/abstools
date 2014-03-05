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
 
class (Interf1_ a) => Interf2_ a where
         
        method2 :: Bool -> Interf2 -> ABS a ()
         
        method3 :: Int -> Interf2 -> ABS a Bool
 
data Interf2 = forall a . (Interf2_ a) => Interf2 (ObjectRef a)
 
instance Sub Interf2 Interf2 where
        up x = x
 
instance Sub Interf2 AnyObject where
        up (Interf2 a) = AnyObject a
 
instance Sub Interf2 Interf1 where
        up (Interf2 a) = Interf1 a
method2_sync b (__wrapped@(Interf2 __obj@(ObjectRef __ioref _)))
  = do __hereCOG <- thisCOG
       __obj1 <- lift (lift (readIORef __ioref))
       otherCOG <- whereis __obj1
       when (__hereCOG /= otherCOG)
         (error "Sync Call on a different COG detected")
       mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
         (method2 b __wrapped)
method2_async b (__wrapped@(Interf2 __obj@(ObjectRef __ioref _)))
  = do __obj1 <- lift (lift (readIORef __ioref))
       __loc <- whereis __obj1
       __mvar <- lift (lift newEmptyMVar)
       AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
       astate@(AState{aCounter = __counter}) <- lift RWS.get
       lift (RWS.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __tid cog __counter
       lift
         (lift (writeChan __loc (RunJob __obj __f (method2 b __wrapped))))
       return __f
method3_sync z (__wrapped@(Interf2 __obj@(ObjectRef __ioref _)))
  = do __hereCOG <- thisCOG
       __obj1 <- lift (lift (readIORef __ioref))
       otherCOG <- whereis __obj1
       when (__hereCOG /= otherCOG)
         (error "Sync Call on a different COG detected")
       mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
         (method3 z __wrapped)
method3_async z (__wrapped@(Interf2 __obj@(ObjectRef __ioref _)))
  = do __obj1 <- lift (lift (readIORef __ioref))
       __loc <- whereis __obj1
       __mvar <- lift (lift newEmptyMVar)
       AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
       astate@(AState{aCounter = __counter}) <- lift RWS.get
       lift (RWS.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __tid cog __counter
       lift
         (lift (writeChan __loc (RunJob __obj __f (method3 z __wrapped))))
       return __f
 
data Class1 = Class1{class1_loc :: (Object__ o) => ABS o COG,
                     class1_p1 :: Int, class1_p2 :: Int, class1_p3 :: Int,
                     class1_x :: Int, class1_o2 :: Interf2}
__class1 p1 p2 p3
  = Class1{class1_p1 = p1, class1_p2 = p2, class1_p3 = p3}
 
instance Object__ Class1 where
        new __cont
          = do __chan <- lift (lift newChan)
               let __x = 0
               let __o2 = null
               let __c
                     = __cont{class1_x = __x, class1_o2 = __o2,
                              class1_loc = return __chan}
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
               let __o2 = null
               let __c
                     = __cont{class1_x = __x, class1_o2 = __o2, class1_loc = thisCOG}
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
 
set_class1_o2 :: Interf2 -> ABS Class1 ()
set_class1_o2 v
  = do (AConf this@(ObjectRef ioref _) thisCOG _) <- lift RWS.ask
       astate@(AState _ om) <- lift RWS.get
       lift (lift (modifyIORef' ioref (\ c -> c{class1_o2 = v})))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 4) om
       maybe (return ())
         (\ woken -> lift (lift (writeList2Chan thisCOG woken)))
         maybeWoken
       lift (RWS.put astate{aSleepingO = om'})
 
instance Interf1_ Class1 where
        method1 n m this = do return (n + m)
 
instance Interf2_ Class1 where
        method2 b this = return ()
        method3 z this
          = do z <- return (z + 1)
               z <- return ((2 + z) + 3)
               z <- (readThis >>=
                       \ Class1{class1_x = __x} -> return ((__x + 4) + 5))
               y <- (readThis >>=
                       \ Class1{class1_x = __x} -> (method1_sync __x 4 (up this)))
                      :: ABS Class1 Int
               (set_class1_x =<< (return ((y) + 1)))
               (set_class1_x =<<
                  ((readThis >>= \ Class1{class1_x = __x} -> return (__x + (y)))))
               (readThis >>= \ Class1{class1_x = __x} -> return (__x == 1))
mainABS
  = do o1 <- liftM Interf2 (new (__class1 1 2 3)) :: ABS Top Interf2
       (method1_sync 1 3 (up o1))
       (method2_sync True (up o1))
       f <- (method1_async 1 4 (up o1)) :: ABS Top (Fut Int)
       await (FutureGuard f)
       f2 <- (method2_async True (up o1)) :: ABS Top (Fut ())
       await (FutureGuard f2)
       res <- get (f) :: ABS Top Int
       res_ <- (method3_sync (res) (up o1)) :: ABS Top Bool
       assert (return ((res) == 3))
main = main_is mainABS
