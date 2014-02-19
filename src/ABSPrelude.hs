{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}

module ABSPrelude 
    (module Base,
     module Prim,
     module Core,
     Prelude.return,
     lift, liftM,
     newIORef, modifyIORef',
     newChan, writeList2Chan,
     updateLookupWithKey,
     ifthenM, ifthenelseM, notM, negateM,
     nil, cons,
     (Prelude.=<<), (Prelude.>>=), Prelude.Maybe (..), Prelude.maybe,
     Prelude.Int, Prelude.Bool (..) , List,
     (Prelude.||), (Prelude.&&), (Prelude.==), (Prelude./=), (Prelude.<), (Prelude.<=), (Prelude.>=), (Prelude.>), (Prelude.+), (Prelude.-), (Prelude.*), (/), (%),
     (||:), (&&:), (==:), (/=:), (<:), (<=:), (>=:), (>:), (+:), (-:), (*:), (/:), (%:) 
    )
        where

import qualified Prelude as Prelude
import Base
import Prim
import Core

import Control.Monad.Trans.Class (lift)
import Control.Monad (when, liftM, liftM2)
import Control.Exception.Base (assert, evaluate)
import Data.IORef (newIORef, modifyIORef')
import Control.Concurrent (newChan, writeList2Chan)
import Data.Map.Strict (updateLookupWithKey)


class IntOrRational a where
    (/) :: a -> a -> a

instance IntOrRational (Prelude.Int) where
    (/) = Prelude.div

instance IntOrRational (Prelude.Rational) where
    (/) = (Prelude./)

x % y = Prelude.fromIntegral (x `Prelude.mod` y)

-- data List a = Nil | Cons a (List a)  -- not this, we want to map to actual Haskell lists

type List a = [a]

-- so smart constructors for this
nil = []
cons = (:)

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
