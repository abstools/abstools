{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude, Rank2Types #-}

module ABSPrelude 
    (module Base,
     module Core,
     module Utils,
     Prelude.return, Exception.evaluate, Prelude.error,
     lift, liftM,
     newIORef, modifyIORef', readIORef, when, mapMonad,
     newChan, writeChan, writeList2Chan, newEmptyMVar,
     M.updateLookupWithKey,
     ifthenM, ifthenelseM, notM, negateM,
     assert, 
     Pair, Prelude.fst, Prelude.snd, Triple, fstT, sndT, trd,
     null,
     (Prelude.=<<), (Prelude.>>=), Prelude.Maybe (..), Prelude.Either (..), left, right, Prelude.maybe, fromJust, Prelude.fromIntegral,
     Prelude.Int, Prelude.Rational, Prelude.Bool (..) , Prelude.Eq, List, Prelude.String,
     (Prelude.||), (Prelude.&&), (Prelude.==), (Prelude./=), (Prelude.<), (Prelude.<=), (Prelude.>=), (Prelude.>), (Prelude.+), (Prelude.-), (Prelude.*), (/), (%),
     (||:), (&&:), (==:), (/=:), (<:), (<=:), (>=:), (>:), (+:), (-:), (*:), (/:), (%:),
     M.Map, M.empty, put, insertAssoc, lookupUnsafe, removeKey,
     length,
     listArray, replace, elemAt, Prelude.repeat, Array
    )
        where

import qualified Prelude as Prelude
import Base
import Core
import Utils

import Control.Monad.Trans.Class (lift)
import Control.Monad (when, liftM, liftM2)
import qualified Control.Exception.Base as Exception (evaluate)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Control.Concurrent (newChan, writeChan, writeList2Chan, newEmptyMVar)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Control.Monad.Coroutine (mapMonad)
import qualified Data.Array.Unboxed as UArray
import Data.Array.Unboxed (listArray)
import Data.List (length)

class IntOrRational a where
    (/) :: a -> a -> a

instance IntOrRational (Prelude.Int) where
    (/) = Prelude.div

instance IntOrRational (Prelude.Rational) where
    (/) = (Prelude./)

x % y = Prelude.fromIntegral (x `Prelude.mod` y)

-- data List a = Nil | Cons a (List a)  -- not this, we want to map to actual Haskell lists

type List a = [a]

ifthenM :: (Object__ o) => ABS o (Prelude.Bool) -> ABS o () -> ABS o ()
ifthenM texp stm_then = texp Prelude.>>= (\ e -> when e stm_then)

ifthenelseM :: (Object__ o) => ABS o (Prelude.Bool) -> ABS o a -> ABS o a -> ABS o a
ifthenelseM texp stm_then stm_else = texp Prelude.>>= (\ e -> if e 
                                                            then stm_then
                                                            else stm_else)

(||:) :: (Object__ o) => ABS o Prelude.Bool -> ABS o Prelude.Bool -> ABS o Prelude.Bool
(||:) = liftM2 (Prelude.||)

(&&:) :: (Object__ o) => ABS o Prelude.Bool -> ABS o Prelude.Bool -> ABS o Prelude.Bool
(&&:) = liftM2 (Prelude.&&)

(==:) :: (Prelude.Eq a, Object__ o) => ABS o a -> ABS o a -> ABS o Prelude.Bool
(==:) = liftM2 (Prelude.==)

(/=:) :: (Prelude.Eq a, Object__ o) => ABS o a -> ABS o a -> ABS o Prelude.Bool
(/=:) = liftM2 (Prelude./=)

(<:) :: (Prelude.Ord a, Object__ o) => ABS o a -> ABS o a -> ABS o Prelude.Bool
(<:) = liftM2 (Prelude.<)

(<=:) :: (Prelude.Ord a, Object__ o) => ABS o a -> ABS o a -> ABS o Prelude.Bool
(<=:) = liftM2 (Prelude.<=)

(>=:) :: (Prelude.Ord a, Object__ o) => ABS o a -> ABS o a -> ABS o Prelude.Bool
(>=:) = liftM2 (Prelude.>=)

(>:) :: (Prelude.Ord a, Object__ o) => ABS o a -> ABS o a -> ABS o Prelude.Bool
(>:) = liftM2 (Prelude.>)

(+:) :: (Prelude.Num a, Object__ o) => ABS o a -> ABS o a -> ABS o a
(+:) = liftM2 (Prelude.+)

(-:) :: (Prelude.Num a, Object__ o) => ABS o a -> ABS o a -> ABS o a
(-:) = liftM2 (Prelude.-)

(*:) :: (Prelude.Num a, Object__ o) => ABS o a -> ABS o a -> ABS o a
(*:) = liftM2 (Prelude.*)

(/:) :: (IntOrRational a, Object__ o) => ABS o a -> ABS o a -> ABS o a
(/:) = liftM2 (/)

(%:) :: (Prelude.Integral a, Prelude.Num b, Object__ o) => ABS o a -> ABS o a -> ABS o b
(%:) = liftM2 (%)

notM :: Object__ o => ABS o Prelude.Bool -> ABS o Prelude.Bool
notM = liftM (Prelude.not)

negateM :: (Prelude.Num a, Object__ o) => ABS o a -> ABS o a
negateM = liftM (Prelude.negate)

assert :: (Object__ o) => ABS o Prelude.Bool -> ABS o ()
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

null = Prelude.undefined

-- arrays

replace a cs = a UArray.// cs
elemAt(a, i) = a UArray.! i

type Array = UArray.UArray
