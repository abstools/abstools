{-# LANGUAGE ExistentialQuantification, Rank2Types, EmptyDataDecls #-}

module Base where

import Data.IORef (IORef)
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Chan (Chan)
import qualified Data.Map.Strict as M (Map)
import qualified Control.Monad.Trans.RWS as RWS (RWST)

data FutureRef a = FutureRef (MVar a) ThreadId Int

data AnyFutureRef = forall a. AnyFuture (FutureRef a)

data AnyObjectRef = forall o. Object_ o => AnyObject (ObjectRef o)

data Result a = R a
                | forall o b . F (FutureRef b) (ObjectRef o) (ABS o a a) (FutureRef a)
                | forall o. Object_ o => T (ObjectRef o) [Int] (ABS o a a) (FutureRef a)

data Job = forall a o . RunJob (Object_ o => ObjectRef o) (ABS o a a) (FutureRef a)
         | forall a . WakeupJob (FutureRef a)

type FutureMap = M.Map AnyFutureRef [Job]

type ObjectMap = M.Map (AnyObjectRef, Int) [Job]


data AConf o a = AConf {
      aThis :: (Object_ o) => ObjectRef o,
      aCOG  :: COG,
      aThread :: ThreadId,
      aFut :: FutureRef a
    }


data AState = AState {
      aCounter :: Int,
      aSleepingO :: ObjectMap
    }

type ABS o f r = RWS.RWST (AConf o f) ()  AState IO (Result r)

type COG = Chan Job

class Object_ a where
    new :: (Object_ o) => a -> ABS o f (ObjectRef a)
    new_local :: a -> (Object_ o) => ABS o f (ObjectRef a)
    __init :: ObjectRef a -> ABS a f  () 
    __init _ = return (R ())     -- default implementation of init
    run :: ObjectRef a -> ABS a f  () 
    run _ = return (R ())        -- default implementation of run
    whereis :: (Object_ o) => a -> ABS o b COG

type Object = forall a. (Object_ a) => ObjectRef a

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

data AwaitGuard o a = forall b. FutureGuard (FutureRef b)
                    | ThisGuard [Int] (ABS o a Bool)
                    | AwaitGuard o a :&: AwaitGuard o a

data Top

instance Object_ Top where
    new = error "cannot instantiated the top-level Top"
    new_local = error "cannot instantiated the top-level Top"
    whereis = error "top-level is not an object"


