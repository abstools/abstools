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
         
        method3 :: Int -> Interf1 -> Interf2 -> ABS a Bool
 
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
method3_sync z k (__wrapped@(Interf2 __obj@(ObjectRef __ioref _)))
  = do __hereCOG <- thisCOG
       __obj1 <- lift (lift (readIORef __ioref))
       otherCOG <- whereis __obj1
       when (__hereCOG /= otherCOG)
         (error "Sync Call on a different COG detected")
       mapMonad (withReaderT (\ aconf -> aconf{aThis = __obj}))
         (method3 z k __wrapped)
method3_async z k (__wrapped@(Interf2 __obj@(ObjectRef __ioref _)))
  = do __obj1 <- lift (lift (readIORef __ioref))
       __loc <- whereis __obj1
       __mvar <- lift (lift newEmptyMVar)
       AConf{aThread = __tid, aCOG = cog} <- lift RWS.ask
       astate@(AState{aCounter = __counter}) <- lift RWS.get
       lift (RWS.put (astate{aCounter = __counter + 1}))
       let __f = FutureRef __mvar __tid cog __counter
       lift
         (lift (writeChan __loc (RunJob __obj __f (method3 z k __wrapped))))
       return __f
 
class (Interf2_ a, Interf1_ a) => Interf3_ a
 
data Interf3 = forall a . (Interf3_ a) => Interf3 (ObjectRef a)
 
instance Sub Interf3 Interf3 where
        up x = x
 
instance Sub Interf3 AnyObject where
        up (Interf3 a) = AnyObject a
 
instance Sub Interf3 Interf2 where
        up (Interf3 a) = Interf2 a
 
instance Sub Interf3 Interf1 where
        up (Interf3 a) = Interf1 a
 
class (Interf1_ a) => Interf4_ a
 
data Interf4 = forall a . (Interf4_ a) => Interf4 (ObjectRef a)
 
instance Sub Interf4 Interf4 where
        up x = x
 
instance Sub Interf4 AnyObject where
        up (Interf4 a) = AnyObject a
 
instance Sub Interf4 Interf1 where
        up (Interf4 a) = Interf1 a