{-# LANGUAGE Rank2Types, NoImplicitPrelude, ImpredicativeTypes,
  LiberalTypeSynonyms #-}
module Main where
import qualified Control.Monad.Trans.RWS as RWS
import ABSPrelude
 
class (Object_ a) => Interf1_ a where
         
        method1 :: Int -> Int -> ObjectRef a -> ABS a Int
 
data Interf1 = forall a . (Interf1_ a) => Interf1 (ObjectRef a)
 
class (Interf1_ a) => Interf2_ a where
         
        method2 :: Bool -> ObjectRef a -> ABS a ()
         
        method3 :: Int -> ObjectRef a -> ABS a Bool
 
data Interf2 = forall a . (Interf2_ a) => Interf2 (ObjectRef a)
 
data Class1 = Class1{class1_loc :: (Object_ o) => ABS o COG,
                     class1_p1 :: Int, class1_p2 :: Int, class1_p3 :: Int,
                     class1_x :: Int, class1_o2 :: Interf2}
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
          = do return 3
               z <- return (z + 1)
               z <- return (2 + z + 3)
               z <- readObject this >>=
                      \ Class1{class1_x = __x} -> return (__x + 4 + 5)
               y <- readObject this >>=
                      \ Class1{class1_x = __x} -> this `sync_call` method1 __x 4
               set_class1_x =<< (return (y + 1))
               set_class1_x =<<
                 (readObject this >>= \ Class1{class1_x = __x} -> return (__x + y))
               readObject this >>= \ Class1{class1_x = __x} -> return (__x == 1)
mainABS
  = do o1 <- new (class1 1 2 3)
       o1 `sync_call` method1 1 3
       o1 `sync_call` method2 True
       f <- o1 `async_call` method1 1 4
       await (FutureGuard f)
       f2 <- o1 `async_call` method2 True
       await (FutureGuard f2)
       res <- get (return f)
       res_ <- o1 `sync_call` method3 res
       assert (return (res == 3))
main = main_is mainABS
