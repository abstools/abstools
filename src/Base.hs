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

data Fut a = FutureRef (MVar a) COG Int
           |  TopRef            -- to reply back to the main block
data AnyFuture = forall a. AnyFuture (Fut a)

data AnyObject = forall o. Object__ o => AnyObject (ObjectRef o)

data ObjectRef a = ObjectRef (IORef a) Int ThreadId
                 | NullRef
                 deriving Eq

instance Eq AnyFuture where
    AnyFuture (FutureRef _ cid1 id1) == AnyFuture (FutureRef _ cid2 id2) = id1 == id2 && cid1 == cid2

instance Ord AnyFuture where
    compare (AnyFuture (FutureRef _ (_, tid1) id1)) (AnyFuture (FutureRef _ (_, tid2) id2)) = compare (tid1,id1) (tid2,id2)

-- this is for ordering inside the cog but can be used also for root type equality if we expose to the ABS language the AnyObject interface type
instance Eq AnyObject where
    AnyObject (ObjectRef _ id1 tid1) == AnyObject (ObjectRef _ id2 tid2) = tid1 == tid2 && id1 == id2

-- alias with above method for easier code generation
__eqAnyObject :: AnyObject -> AnyObject -> Bool
__eqAnyObject = (==)

-- this is for ordering inside the cog
instance Ord AnyObject where
    compare (AnyObject (ObjectRef _ _ id1)) (AnyObject (ObjectRef _ _ id2)) = compare id1 id2

type FutureMap = M.Map AnyFuture [Job]

type ObjectMap = M.Map (AnyObject, Int) [Job]

type COG = (Chan Job, ThreadId)

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

-- the null class object, 
-- it does not have any constructors
-- it is just for typing
data Null -- no constructor, possible needs a constructor

instance Object__ Null where
    new = error "cannot instantiated null"
    new_local = error "cannot instantiated null"
    whereis = error "null is not related to a COG"

data AwaitOn = S
             | forall f. F (Fut f)
             | forall o. Object__ o => T (ObjectRef o) [Int]

data Job = forall o a . Object__ o => RunJob (ObjectRef o) (Fut a) (ABS o a)
         | forall f . WakeupJob (Fut f)



data AConf o = AConf {
      aThis :: (Object__ o) => ObjectRef o,
      aCOG  :: COG
    }


data AState = AState {
      aCounter :: Int,
      aSleepingO :: ObjectMap
    }

type ABS o r = Coroutine (Yield AwaitOn) (RWS.RWST (AConf o) ()  AState IO) r


class Sub sub sup where
    up :: sub -> sup
