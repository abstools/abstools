{-# LANGUAGE Rank2Types, TypeFamilies #-}

module Concept where

import Base
import Core
import Utils
import Prim
import ABSPrelude


import Data.IORef
import Control.Monad (liftM, liftM2)
import Control.Concurrent (newChan, writeList2Chan)
import qualified Control.Monad.Trans.RWS as RWS
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as M

class (Object_ a) => Interf1_ a where
    method1 :: Int -> Int -> ObjectRef a -> ABS a Int

class (Interf1_ a) => Interf2_ a where
    method2 :: Bool -> ObjectRef a -> ABS a ()
    method3 :: Int -> ObjectRef a -> ABS a Bool

data Class1 = Class1 {
      -- generate increasing counter tags for identifying object fields/attrs
      class1_p1 :: Int,
      class1_p2 :: Int,
      class1_p3 :: Int,
      class1_x ::  Int,
      class1_o2 :: Interf2,
      class1_loc :: (Object_ o) => ABS o COG
    }

type Interf2 = forall a. (Interf2_ a) => ObjectRef a

-- smart constructor
-- passing the class parameters
class1 p1 p2 p3 = Class1 {class1_p1 = p1, class1_p2 = p2, class1_p3 = p3}

-- generate these with Template Haskell

set_class1_p1 act = do
 v <- act
 (AConf this@(ObjectRef ioref _) thisCOG _ ) <- lift $ RWS.ask
 astate@(AState _ om) <- lift $ RWS.get
 lift $ lift $ modifyIORef' ioref (\ c -> c {class1_p1 = v})      -- update the field value
 let (maybeWoken, om') = M.updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 0) om
 maybe (return ()) (\ woken -> lift $ lift $ writeList2Chan thisCOG woken) maybeWoken
 lift $ RWS.put $ astate {aSleepingO = om'}  -- remove the woken up processes from the sleeping queue

set_class1_x v = do
 (AConf this@(ObjectRef ioref _) thisCOG _) <- lift $RWS.ask
 astate@(AState _ om) <- lift $ RWS.get
 lift $ lift $ modifyIORef' ioref (\ c -> c {class1_x = v})      -- update the field value
 let (maybeWoken, om') = M.updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 3) om
 maybe (return ()) (\ woken -> lift $ lift $ writeList2Chan thisCOG woken) maybeWoken
 lift $ RWS.put $ astate {aSleepingO = om'}  -- remove the woken up processes from the sleeping queue

instance Object_ Class1 where
    new cont = do
               __chan <- lift $ lift $ newChan
               let __x = 0
               let __c = cont { class1_x = __x, class1_loc = (return __chan) }
               __ioref <- lift $ lift $ newIORef __c
               let __obj = ObjectRef __ioref 0
               lift $ lift $ spawnCOG __chan
               __obj `async_call` __init
               __obj `async_call` run
               return $ __obj
    new_local cont = do
      let __x = 0
      let __c = cont {class1_x = __x, class1_loc = thisCOG}
      __ioref <- lift $ lift $ newIORef __c
      __astate@(AState {aCounter = __counter})  <- lift $ RWS.get
      lift $ RWS.put (__astate {aCounter = __counter + 1})
      let __obj = ObjectRef __ioref __counter
      __obj `sync_call` __init
      __obj `sync_call` run
      return $ __obj

    __init this = do
      set_class1_x =<< (return 1)
      set_class1_x =<< (liftM2 (+) ((liftM class1_x) (readObject this)) (return 1))


    whereis = class1_loc



instance Interf1_ Class1 where
    method1 n m this = do
      return (n+m);

instance Interf2_ Class1 where
    method2 b this = return (())
    method3 z this = do
      await (ThisGuard [1, 2, 3]
                          ((liftM class1_x (readObject this) +: (liftM class1_p2 (readObject this)))
                           >: (liftM class1_p3 (readObject this))))
      z <- liftM2 (+) (return z) (return 1)
      z <- liftM2 (+) (liftM2 (+) (return 2) (return z)) (return 3)
      z <- liftM2 (+) (liftM2 (+) ((liftM class1_x) (readObject this)) (return 4)) (return 5)
      y <- this `sync_call` (method1 1 4)
      set_class1_x =<< (liftM2 (+) (return y) (return 1))
      set_class1_x =<< this `sync_call` (method1 1 4)
      set_class1_x =<< (liftM2 (+) ((liftM class1_x) (readObject this)) (return y))
      -- Class1 {class1_p1 = this_p1, class1_p2 = this_p2} <- lift $ readObject this
      liftM2 (==) ((liftM class1_x) (readObject this)) (return 1)


method4_no_interf :: (Object_ a) => ObjectRef Class1 -> ABS a ()
method4_no_interf this = do
  return (())


-- generated
mainABS :: ABS Top ()
mainABS = do
  o1 <- new (class1 1 2 3)
  o1 `sync_call` (method1 1 3)
  o1 `sync_call` (method2 True)
  f <- o1 `async_call` (method1 1 4)
  await (FutureGuard f)
  f2 <- o1 `async_call` (method2 True)
  await (FutureGuard f2)
  res <- get (return f)
  res_ <- o1 `sync_call` (method3 res)
  return (()) -- add at the end of the generated main because main has to return ()



