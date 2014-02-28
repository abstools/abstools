{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude, Rank2Types #-}

module ABSPrelude 
    (module Base,
     module Core,
     Prelude.return, Exception.evaluate,
     lift, liftM,
     newIORef, modifyIORef',
     newChan, writeList2Chan,
     M.updateLookupWithKey,
     ifthenM, ifthenelseM, notM, negateM,
     assert, 
     Pair, Prelude.fst, Prelude.snd, Triple, fstT, sndT, trd,
     (Prelude.=<<), (Prelude.>>=), Prelude.Maybe (..), Prelude.Either (..), left, right, Prelude.maybe, fromJust,
     Prelude.Int, Prelude.Bool (..) , Prelude.Eq, List,
     (Prelude.||), (Prelude.&&), (Prelude.==), (Prelude./=), (Prelude.<), (Prelude.<=), (Prelude.>=), (Prelude.>), (Prelude.+), (Prelude.-), (Prelude.*), (/), (%),
     (||:), (&&:), (==:), (/=:), (<:), (<=:), (>=:), (>:), (+:), (-:), (*:), (/:), (%:),
     M.Map, M.empty, put, insertAssoc, lookupUnsafe, removeKey
    )
        where

import qualified Prelude as Prelude
import Base
import Core

import Control.Monad.Trans.Class (lift)
import Control.Monad (when, liftM, liftM2)
import qualified Control.Exception.Base as Exception (evaluate)
import Data.IORef (newIORef, modifyIORef')
import Control.Concurrent (newChan, writeList2Chan)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)


class IntOrRational a where
    (/) :: a -> a -> a

instance IntOrRational (Prelude.Int) where
    (/) = Prelude.div

instance IntOrRational (Prelude.Rational) where
    (/) = (Prelude./)

x % y = Prelude.fromIntegral (x `Prelude.mod` y)

-- data List a = Nil | Cons a (List a)  -- not this, we want to map to actual Haskell lists

type List a = [a]

ifthenM :: Prelude.Monad m => m (Prelude.Bool) -> m () -> m ()
ifthenM texp stm_then = texp Prelude.>>= (\ e -> when e stm_then)

ifthenelseM :: Prelude.Monad m => m (Prelude.Bool) -> m a -> m a -> m a
ifthenelseM texp stm_then stm_else = texp Prelude.>>= (\ e -> if e 
                                                            then stm_then
                                                            else stm_else)

(||:) :: Prelude.Monad m => m Prelude.Bool -> m Prelude.Bool -> m Prelude.Bool
(||:) = liftM2 (Prelude.||)

(&&:) :: Prelude.Monad m => m Prelude.Bool -> m Prelude.Bool -> m Prelude.Bool
(&&:) = liftM2 (Prelude.&&)

(==:) :: (Prelude.Eq a, Prelude.Monad m) => m a -> m a -> m Prelude.Bool
(==:) = liftM2 (Prelude.==)

(/=:) :: (Prelude.Eq a, Prelude.Monad m) => m a -> m a -> m Prelude.Bool
(/=:) = liftM2 (Prelude./=)

(<:) :: (Prelude.Ord a, Prelude.Monad m) => m a -> m a -> m Prelude.Bool
(<:) = liftM2 (Prelude.<)

(<=:) :: (Prelude.Ord a, Prelude.Monad m) => m a -> m a -> m Prelude.Bool
(<=:) = liftM2 (Prelude.<=)

(>=:) :: (Prelude.Ord a, Prelude.Monad m) => m a -> m a -> m Prelude.Bool
(>=:) = liftM2 (Prelude.>=)

(>:) :: (Prelude.Ord a, Prelude.Monad m) => m a -> m a -> m Prelude.Bool
(>:) = liftM2 (Prelude.>)

(+:) :: (Prelude.Num a, Prelude.Monad m) => m a -> m a -> m a
(+:) = liftM2 (Prelude.+)

(-:) :: (Prelude.Num a, Prelude.Monad m) => m a -> m a -> m a
(-:) = liftM2 (Prelude.-)

(*:) :: (Prelude.Num a, Prelude.Monad m) => m a -> m a -> m a
(*:) = liftM2 (Prelude.*)

(/:) :: (IntOrRational a, Prelude.Monad m) => m a -> m a -> m a
(/:) = liftM2 (/)

(%:) :: (Prelude.Integral a, Prelude.Num b, Prelude.Monad m) => m  a -> m a -> m b
(%:) = liftM2 (%)

notM :: Prelude.Monad m => m Prelude.Bool -> m Prelude.Bool
notM = liftM (Prelude.not)

negateM :: (Prelude.Num a, Prelude.Monad m) => m a -> m a
negateM = liftM (Prelude.negate)

assert :: (Object_ o) => ABS o Prelude.Bool -> ABS o ()
assert act = act Prelude.>>= \ pred -> when (Prelude.not pred) (Prelude.error "Assertion failed")


put :: Prelude.Ord k => M.Map k v -> k -> v -> M.Map k v
put m k v = M.insert k v m

-- turned an unsafe to a safe operation
insertAssoc :: Prelude.Ord k => (k,v) -> M.Map k v -> M.Map k v
insertAssoc (k,v) m = M.insert k v m

lookupUnsafe :: Prelude.Ord k => M.Map k v -> k -> v
lookupUnsafe m k = m M.! k

removeKey :: Prelude.Ord k => M.Map k v -> k -> M.Map k v
removeKey = Prelude.flip M.delete

type Pair a b = (a,b)

type Triple a b c = (a,b,c)

left (Prelude.Left a ) = a
right (Prelude.Right a) = a


fstT (a,_,_) = a
sndT (_,b,_) = b
trd (_,_,c) = c

