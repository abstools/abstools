{-# LANGUAGE ExistentialQuantification, Rank2Types, EmptyDataDecls #-}

module Base where

import Data.IORef (IORef)
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Chan (Chan)
import qualified Data.Map.Strict as M (Map)
import qualified Control.Monad.Trans.RWS as RWS (RWST)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield)

data FutureRef a = FutureRef (MVar a) ThreadId Int
                 | TopRef

data AnyFutureRef = forall a. AnyFuture (FutureRef a)

data AnyObjectRef = forall o. Object_ o => AnyObject (ObjectRef o)

data ObjectRef a = ObjectRef (IORef a) Int
                 deriving Eq

instance Eq AnyFutureRef where
    AnyFuture (FutureRef _ tid1 id1) == AnyFuture (FutureRef _ tid2 id2) = id1 == id2 && tid1 == tid2

instance Ord AnyFutureRef where
    compare (AnyFuture (FutureRef _ tid1 id1)) (AnyFuture (FutureRef _ tid2 id2)) = compare (tid1,id1) (tid2,id2)

instance Eq AnyObjectRef where
    AnyObject (ObjectRef _ id1) == AnyObject (ObjectRef _ id2) = id1 == id2

instance Ord AnyObjectRef where
    compare (AnyObject (ObjectRef _ id1)) (AnyObject (ObjectRef _ id2)) = compare id1 id2

type FutureMap = M.Map AnyFutureRef [Job]

type ObjectMap = M.Map (AnyObjectRef, Int) [Job]

type COG = Chan Job

class Object_ a where
    new :: (Object_ o) => a -> ABS o (ObjectRef a)
    new_local :: a -> (Object_ o) => ABS o (ObjectRef a)
    __init :: ObjectRef a -> ABS a () 
    __init _ = return (())     -- default implementation of init
    run :: ObjectRef a -> ABS a () 
    run _ = return (())        -- default implementation of run
    whereis :: (Object_ o) => a -> ABS o COG

type Object = forall a. (Object_ a) => ObjectRef a


data AwaitGuard o = forall b. FutureGuard (FutureRef b)
                  | ThisGuard [Int] (ABS o Bool)
                  | AwaitGuard o :&: AwaitGuard o

data Top

instance Object_ Top where
    new = error "cannot instantiated the top-level Top"
    new_local = error "cannot instantiated the top-level Top"
    whereis = error "top-level is not an object"

data AwaitOn = S
             | forall f. F (FutureRef f)
             | forall o. Object_ o => T (ObjectRef o) [Int]

data Job = forall o a . Object_ o => RunJob (ObjectRef o) (FutureRef a) (ABS o a)
         | forall f . WakeupJob (FutureRef f)



data AConf o = AConf {
      aThis :: (Object_ o) => ObjectRef o,
      aCOG  :: COG,
      aThread :: ThreadId
    }


data AState = AState {
      aCounter :: Int,
      aSleepingO :: ObjectMap
    }

type ABS o r = Coroutine (Yield AwaitOn) (RWS.RWST (AConf o) ()  AState IO) r




