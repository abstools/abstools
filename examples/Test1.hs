{-# LANGUAGE Rank2Types, NoImplicitPrelude, ImpredicativeTypes,
  LiberalTypeSynonyms #-}
module Test1 where
import qualified Control.Monad.Trans.RWS as RWS
import ABSPrelude
 
class (Object_ a) => Interf1_ a where
         
        method1 :: Int -> Int -> ObjectRef a -> ABS a Int
 
type Interf1 = forall a . (Interf1_ a) => ObjectRef a
 
data Class1 = Class1{class1_loc :: (Object_ o) => ABS o COG,
                     class1_p1 :: Int, class1_p2 :: Int, class1_p3 :: Int,
                     class1_x :: Int}
class1 p1 p2 p3
  = Class1{class1_p1 = p1, class1_p2 = p2, class1_p3 = p3}
 
instance Object_ Class1 where
        new __cont
          = do __chan <- lift (lift newChan)
               let __x = 0
               let __c = __cont{class1_x = __x, class1_loc = return __chan}
               __ioref <- lift (lift (newIORef __c))
               let __obj = ObjectRef __ioref 0
               lift (lift (spawnCOG __chan))
               __obj `async_call` __init
               __obj `async_call` run
               return __obj
        new_local __cont
          = do let __x = 0
               let __c = __cont{class1_x = __x, class1_loc = thisCOG}
               __ioref <- lift (lift (newIORef __c))
               __astate@(AState{aCounter = __counter}) <- lift RWS.get
               lift (RWS.put (__astate{aCounter = __counter + 1}))
               let __obj = ObjectRef __ioref __counter
               __obj `sync_call` __init
               __obj `sync_call` run
               return __obj
        whereis = class1_loc
        __init this
          = do set_class1_x =<< (return 1)
               set_class1_x =<<
                 (readObject this >>= \ Class1{class1_x = __x} -> return (__x + 1))
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
  = do o1 <- new (class1 1 2 3)
       o2 <- new (class1 3 4 5)
       f1 <- o1 `async_call` method1 3 4
       await (FutureGuard f1)
       res1 <- get (return f1)
       assert (return (res1 == 7))
       f2 <- o2 `async_call` method1 4 5
       await (FutureGuard f2)
       res2 <- get (return f2)
       assert (return (res2 == 9))
main = main_is mainABS