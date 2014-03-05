{-# LANGUAGE ExistentialQuantification, Rank2Types, EmptyDataDecls, MultiParamTypeClasses #-}

module Base where

import Data.IORef (IORef)
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Chan (Chan)
import qualified Data.Map.Strict as M (Map)
import qualified Control.Monad.Trans.RWS as RWS (RWST)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield)

data Fut a = FutureRef (MVar a) ThreadId COG Int
           |  TopRef
data AnyFuture = forall a. AnyFuture (Fut a)

data AnyObject = forall o. Object__ o => AnyObject (ObjectRef o)

data ObjectRef a = ObjectRef (IORef a) Int
                 deriving Eq

instance Eq AnyFuture where
    AnyFuture (FutureRef _ tid1 _ id1) == AnyFuture (FutureRef _ tid2 _ id2) = id1 == id2 && tid1 == tid2

instance Ord AnyFuture where
    compare (AnyFuture (FutureRef _ tid1 _ id1)) (AnyFuture (FutureRef _ tid2 _ id2)) = compare (tid1,id1) (tid2,id2)

instance Eq AnyObject where
    AnyObject (ObjectRef _ id1) == AnyObject (ObjectRef _ id2) = id1 == id2

instance Ord AnyObject where
    compare (AnyObject (ObjectRef _ id1)) (AnyObject (ObjectRef _ id2)) = compare id1 id2

type FutureMap = M.Map AnyFuture [Job]

type ObjectMap = M.Map (AnyObject, Int) [Job]

type COG = Chan Job

class Object__ a where
    new :: (Object__ o) => a -> ABS o (ObjectRef a)
    new_local :: a -> (Object__ o) => ABS o (ObjectRef a)
    __init :: AnyObject -> ABS a () 
    __init _ = return (())     -- default implementation of init
    __run :: AnyObject -> ABS a () 
    __run _ = return (())        -- default implementation of run
    whereis :: (Object__ o) => a -> ABS o COG

data AwaitGuard o = forall b. FutureGuard (Fut b)
                  | ThisGuard [Int] (ABS o Bool)
                  | AwaitGuard o :&: AwaitGuard o

data Top

instance Object__ Top where
    new = error "cannot instantiated the top-level Top"
    new_local = error "cannot instantiated the top-level Top"
    whereis = error "top-level is not an object"

data AwaitOn = S
             | forall f. F (Fut f)
             | forall o. Object__ o => T (ObjectRef o) [Int]

data Job = forall o a . Object__ o => RunJob (ObjectRef o) (Fut a) (ABS o a)
         | forall f . WakeupJob (Fut f)



data AConf o = AConf {
      aThis :: (Object__ o) => ObjectRef o,
      aCOG  :: COG,
      aThread :: ThreadId
    }


data AState = AState {
      aCounter :: Int,
      aSleepingO :: ObjectMap
    }

type ABS o r = Coroutine (Yield AwaitOn) (RWS.RWST (AConf o) ()  AState IO) r


class Sub sub sup where
    up :: sub -> sup
