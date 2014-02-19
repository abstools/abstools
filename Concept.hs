{-# LANGUAGE Rank2Types, TypeFamilies #-}

module Concept where

import Base
import Core
import Utils
import Prim


import Data.IORef
import Control.Monad (liftM, liftM2)
import Control.Concurrent (newChan, writeList2Chan)
import qualified Control.Monad.Trans.RWS as RWS
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as M

class (Object_ a) => Interf1_ a where
    method1 :: Int -> Int -> ObjectRef a -> ABS a b Int

class (Interf1_ a) => Interf2_ a where
    method2 :: Bool -> ObjectRef a -> ABS a b ()
    method3 :: Int -> ObjectRef a -> ABS a b Bool

data Class1 = Class1 {
      -- generate increasing counter tags for identifying object fields/attrs
      class1_p1 :: Int,
      class1_p2 :: Int,
      class1_p3 :: Int,
      class1_x ::  Int,
      class1_o2 :: Interf2,
      class1_loc :: (Object_ o) => ABS o a COG
    }

type Interf2 = forall a. (Interf2_ a) => ObjectRef a

-- smart constructor
-- passing the class parameters
class1 p1 p2 p3 = Class1 {class1_p1 = p1, class1_p2 = p2, class1_p3 = p3}

-- generate these with Template Haskell

set_class1_p1 (R v) = do
 (AConf this@(ObjectRef ioref _) thisCOG _ _) <- RWS.ask
 astate@(AState _ om) <- RWS.get
 lift $ modifyIORef' ioref (\ c -> c {class1_p1 = v})      -- update the field value
 let (maybeWoken, om') = M.updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 0) om
 maybe (return ()) (\ woken -> lift $ writeList2Chan thisCOG woken) maybeWoken
 RWS.put $ astate {aSleepingO = om'}  -- remove the woken up processes from the sleeping queue
 return (R ())

set_class1_x (R v) = do
 (AConf this@(ObjectRef ioref _) thisCOG _ _) <- RWS.ask
 astate@(AState _ om) <- RWS.get
 lift $ modifyIORef' ioref (\ c -> c {class1_x = v})      -- update the field value
 let (maybeWoken, om') = M.updateLookupWithKey (\ k v -> Nothing) (AnyObject this, 3) om
 maybe (return ()) (\ woken -> lift $ writeList2Chan thisCOG woken) maybeWoken
 RWS.put $ astate {aSleepingO = om'}  -- remove the woken up processes from the sleeping queue
 return (R ())

instance Object_ Class1 where
    new cont = do
               __chan <- lift $ newChan
               let __x = 0
               let __c = cont { class1_x = __x, class1_loc = (return (R __chan)) }
               __ioref <- lift $ newIORef __c
               let __obj = ObjectRef __ioref 0
               lift $ spawnCOG __chan
               __obj `async_call` __init
               __obj `async_call` run
               return $ R __obj
    new_local cont = do
      let __x = 0
      let __c = cont {class1_x = __x, class1_loc = thisCOG}
      __ioref <- lift $ newIORef __c
      __astate@(AState {aCounter = __counter})  <- RWS.get
      RWS.put (__astate {aCounter = __counter + 1})
      let __obj = ObjectRef __ioref __counter
      __obj `sync_call` __init
      __obj `sync_call` run
      return $ R __obj

    __init this = do
      set_class1_x =<< liftM R (return 1)
      set_class1_x =<< liftM R (liftM2 (+) (lift $ (liftM class1_x) (readObject this)) (return 1))




    whereis = class1_loc



instance Interf1_ Class1 where
    method1 n m this = do
      return (R (n+m));

instance Interf2_ Class1 where
    method2 b this = return (R ())
    method3 z this = do
      R z <- liftM R $ liftM2 (+) (return z) (return 1)
      R z <- liftM R $ liftM2 (+) (liftM2 (+) (return 2) (return z)) (return 3)
      R z <- liftM R $ liftM2 (+) (liftM2 (+) (lift $ (liftM class1_x) (readObject this)) (return 4)) (return 5)
      R y <- this `sync_call` (method1 1 4)
      set_class1_x =<< liftM R (liftM2 (+) (return y) (return 1))
      set_class1_x =<< this `sync_call` (method1 1 4)
      set_class1_x =<< liftM R (liftM2 (+) (lift $ (liftM class1_x) (readObject this)) (return y))
      -- Class1 {class1_p1 = this_p1, class1_p2 = this_p2} <- lift $ readObject this
      liftM R $ liftM2 (==) (lift $ (liftM class1_x) (readObject this)) (return 1)


method4_no_interf :: (Object_ a) => ObjectRef Class1 -> ABS a f ()
method4_no_interf this = do
  return (R ())

-- generated
-- class1_new :: (Object o) => Int -> Int -> Int -> ABS o a (ObjectRef Class1)
-- class1_new = \ __p1 __p2 __p3 -> do
--                __chan <- lift $ newChan
--                let __x = 0
--                let __c = Class1 __p1 __p2 __p3 __x undefined (return (R __chan))
--                __ioref <- lift $ newIORef __c
--                let __obj = ObjectRef __ioref 0
--                lift $ spawnCOG __chan
--                __obj `async_call` class1_init
--                -- obj `async_call` class1_run
--                return $ R __obj

-- class1_init :: ObjectRef Class1 -> ABS Class1 f ()
-- class1_init this = do
--   set_class1_x =<< liftM R (return 1)
--   set_class1_x =<< liftM R (liftM2 (+) (lift $ (liftM class1_x) (readObject this)) (return 1))

-- -- class1_run      :: ObjectRef Class1 -> ABS Class1 f ()
-- -- class1_run this = return (R ())


-- -- generated
-- class1_new_local :: (Object o) => Int -> Int -> Int -> ABS o a (ObjectRef Class1)
-- class1_new_local = \ __p1 __p2 __p3 -> do
--                      let __x = 0
--                      let __c = Class1 __p1 __p2 __p3 __x undefined thisCOG
--                      __ioref <- lift $ newIORef __c
--                      __astate@(AState {aCounter = __counter})  <- RWS.get
--                      RWS.put (__astate {aCounter = __counter + 1})
--                      let __obj = ObjectRef __ioref __counter
--                      __obj `sync_call` class1_init
--                      -- obj `sync_call` class1_run
--                      return $ R __obj

-- generated
mainABS :: ABS Top () ()
mainABS = do
  R o1 <- new (class1 1 2 3)
  o1 `sync_call` (method1 1 3)
  o1 `sync_call` (method2 True)
  R f <- o1 `async_call` (method1 1 4)
  await (FutureGuard f) $ do
     R f2 <- o1 `async_call` (method2 True)
     await (FutureGuard f2) $ do
                             R res <- get (return f)
                             res_ <- o1 `sync_call` (method3 res)
                             return (R ()) -- add at the end of the generated main because main has to return R ()



