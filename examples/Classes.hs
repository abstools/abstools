{-# LANGUAGE Rank2Types, NoImplicitPrelude, ImpredicativeTypes,
  LiberalTypeSynonyms #-}
module Classes where
import qualified Control.Monad.Trans.RWS as RWS
import ABSPrelude
 
class (Object_ a) => Interf1_ a where
         
        method1 :: Int -> Int -> ObjectRef a -> ABS a fut Int
 
type Interf1 = forall a . (Interf1_ a) => ObjectRef a
 
class (Interf1_ a) => Interf2_ a where
         
        method2 :: Bool -> ObjectRef a -> ABS a fut ()
         
        method3 :: Int -> ObjectRef a -> ABS a fut Bool
 
type Interf2 = forall a . (Interf2_ a) => ObjectRef a
 
data Class1 = Class1{class1_loc :: (Object_ o) => ABS o a COG,
                     class1_p1 :: Int, class1_p2 :: Int, class1_p3 :: Int,
                     class1_x :: Int, class1_o2 :: Interf2}
class1 p1 p2 p3
  = Class1{class1_p1 = p1, class1_p2 = p2, class1_p3 = p3}
 
instance Object_ Class1 where
        new __cont
          = do __chan <- lift newChan
               let __x = 0
               let __c = __cont{class1_x = __x, class1_loc = return (R __chan)}
               __ioref <- lift (newIORef __c)
               let __obj = ObjectRef __ioref 0
               lift (spawnCOG __chan)
               __obj `async_call` __init
               __obj `async_call` run
               return (R __obj)
        new_local __cont
          = do let __x = 0
               let __c = __cont{class1_x = __x, class1_loc = thisCOG}
               __ioref <- lift (newIORef __c)
               __astate@(AState{aCounter = __counter}) <- RWS.get
               RWS.put (__astate{aCounter = __counter + 1})
               let __obj = ObjectRef __ioref __counter
               __obj `async_call` __init
               __obj `async_call` run
               return (R __obj)
        whereis = class1_loc
        __init this
          = do set_class1_x =<< liftM R (return 1)
               set_class1_x =<<
                 liftM R (lift (liftM class1_x (readObject this)) +: return 1)
               return (R ())
 
set_class1_p1 :: Result Int -> ABS Class1 f ()
set_class1_p1 (R v)
  = do (AConf this@(ObjectRef ioref _) thisCOG _ _) <- RWS.ask
       astate@(AState _ om) <- RWS.get
       lift (modifyIORef' ioref (\ c -> c{class1_p1 = v}))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 0) om
       maybe (return ()) (\ woken -> lift (writeList2Chan thisCOG woken))
         maybeWoken
       RWS.put astate{aSleepingO = om}
       return (R ())
 
set_class1_p2 :: Result Int -> ABS Class1 f ()
set_class1_p2 (R v)
  = do (AConf this@(ObjectRef ioref _) thisCOG _ _) <- RWS.ask
       astate@(AState _ om) <- RWS.get
       lift (modifyIORef' ioref (\ c -> c{class1_p2 = v}))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 1) om
       maybe (return ()) (\ woken -> lift (writeList2Chan thisCOG woken))
         maybeWoken
       RWS.put astate{aSleepingO = om}
       return (R ())
 
set_class1_p3 :: Result Int -> ABS Class1 f ()
set_class1_p3 (R v)
  = do (AConf this@(ObjectRef ioref _) thisCOG _ _) <- RWS.ask
       astate@(AState _ om) <- RWS.get
       lift (modifyIORef' ioref (\ c -> c{class1_p3 = v}))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 2) om
       maybe (return ()) (\ woken -> lift (writeList2Chan thisCOG woken))
         maybeWoken
       RWS.put astate{aSleepingO = om}
       return (R ())
 
set_class1_x :: Result Int -> ABS Class1 f ()
set_class1_x (R v)
  = do (AConf this@(ObjectRef ioref _) thisCOG _ _) <- RWS.ask
       astate@(AState _ om) <- RWS.get
       lift (modifyIORef' ioref (\ c -> c{class1_x = v}))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 3) om
       maybe (return ()) (\ woken -> lift (writeList2Chan thisCOG woken))
         maybeWoken
       RWS.put astate{aSleepingO = om}
       return (R ())
 
set_class1_o2 :: Result Interf2 -> ABS Class1 f ()
set_class1_o2 (R v)
  = do (AConf this@(ObjectRef ioref _) thisCOG _ _) <- RWS.ask
       astate@(AState _ om) <- RWS.get
       lift (modifyIORef' ioref (\ c -> c{class1_o2 = v}))
       let (maybeWoken, om')
             = updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 4) om
       maybe (return ()) (\ woken -> lift (writeList2Chan thisCOG woken))
         maybeWoken
       RWS.put astate{aSleepingO = om}
       return (R ())
 
instance Interf1_ Class1 where
        method1 n m this = do liftM R (return n +: return m)
 
instance Interf2_ Class1 where
        method2 b this = return (R ())
        method3 z this
          = do z <- return z +: return 1
               z <- return 2 +: return z +: return 3
               z <- lift (liftM class1_x (readObject this)) +: return 4 +:
                      return 5
               R y <- lift (readObject this) >>=
                        \ Class1{class1_x = __x} -> this `sync_call` method1 __x 4
               set_class1_x =<< liftM R (return y +: return 1)
               set_class1_x =<<
                 liftM R (lift (liftM class1_x (readObject this)) +: return y)
               liftM R (lift (liftM class1_x (readObject this)) ==: return 1)
mainABS
  = do R o1 <- new (class1 1 2 3)
       o1 `sync_call` method1 1 3
       o1 `sync_call` method2 True
       R f <- o1 `async_call` method1 1 4
       await (FutureGuard f)
         (do R f2 <- o1 `async_call` method2 True
             await (FutureGuard f2)
               (do R res <- get (return f)
                   R res_ <- o1 `sync_call` method3 res
                   return (R ()))
             return (R ()))
       return (R ())
main = main_is mainABS