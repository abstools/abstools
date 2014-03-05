{-# LANGUAGE Rank2Types, NoImplicitPrelude,
  ExistentialQuantification, MultiParamTypeClasses #-}
module Main where
import qualified Control.Monad.Trans.RWS as RWS
import Prim
import ABSPrelude
 
class (Object__ a) => Interf1_ a where
         
        method1 :: Interf1 -> ABS a ()
 
data Interf1 = forall a . (Interf1_ a) => Interf1 (ObjectRef a)
 
instance Sub Interf1 Interf1 where
        up x = x
 
instance Sub Interf1 AnyObject where
        up (Interf1 a) = AnyObject a
method1_sync (__wrapped@(Interf1 __obj@(ObjectRef __ioref _)))
  = do __hereCOG <- thisCOG
       __obj1 <- lift (lift (readIORef __ioref))
       otherCOG <- whereis __obj1
       when (__hereCOG /= otherCOG)
         (error "Sync Call on a different COG detected")
       mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
         (method1 __wrapped)
method1_async (__wrapped@(Interf1 __obj@(ObjectRef __ioref _)))
  = do __obj1 <- lift (lift (readIORef __ioref))
       __loc <- whereis __obj1
       __mvar <- lift (lift newEmptyMVar)
       AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
       astate@(AState{aCounter = __counter}) <- lift RWS.get
       lift (RWS.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __tid cog __counter
       lift
         (lift (writeChan __loc (RunJob __obj __f (method1 __wrapped))))
       return __f
 
class (Interf1_ a) => Interf2_ a
 
data Interf2 = forall a . (Interf2_ a) => Interf2 (ObjectRef a)
 
instance Sub Interf2 Interf2 where
        up x = x
 
instance Sub Interf2 AnyObject where
        up (Interf2 a) = AnyObject a
 
instance Sub Interf2 Interf1 where
        up (Interf2 a) = Interf1 a
 
data Cls1 = Cls1{cls1_loc :: (Object__ o) => ABS o COG}
__cls1 = Cls1{}
 
instance Object__ Cls1 where
        new __cont
          = do __chan <- lift (lift newChan)
               let __c = __cont{cls1_loc = return __chan}
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
          = do let __c = __cont{cls1_loc = thisCOG}
               __ioref <- lift (lift (newIORef __c))
               __astate@(AState{aCounter = __counter}) <- lift RWS.get
               lift (RWS.put (__astate{aCounter = __counter + 1}))
               let __obj = ObjectRef __ioref __counter
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__init (AnyObject __obj))
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__run (AnyObject __obj))
               return __obj
        whereis = cls1_loc
 
instance Interf1_ Cls1 where
        method1 this = return ()
 
data Cls2 = Cls2{cls2_loc :: (Object__ o) => ABS o COG}
__cls2 = Cls2{}
 
instance Object__ Cls2 where
        new __cont
          = do __chan <- lift (lift newChan)
               let __c = __cont{cls2_loc = return __chan}
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
          = do let __c = __cont{cls2_loc = thisCOG}
               __ioref <- lift (lift (newIORef __c))
               __astate@(AState{aCounter = __counter}) <- lift RWS.get
               lift (RWS.put (__astate{aCounter = __counter + 1}))
               let __obj = ObjectRef __ioref __counter
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__init (AnyObject __obj))
               mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
                 (__run (AnyObject __obj))
               return __obj
        whereis = cls2_loc
 
instance Interf1_ Cls2 where
        method1 this = return ()
 
instance Interf2_ Cls2
mainABS
  = do o1 <- liftM Interf1 (new __cls1) :: ABS Top Interf1
       o2 <- liftM Interf2 (new __cls2) :: ABS Top Interf2
       x <- return ((up o1) : ((up o2) : [])) :: ABS Top (List Interf1)
       return ()
main = main_is mainABS