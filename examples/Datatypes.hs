{-# LANGUAGE Rank2Types, NoImplicitPrelude,
  ExistentialQuantification, MultiParamTypeClasses #-}
module Main where
import qualified Control.Monad.Trans.RWS as RWS
import Prim
import ABSPrelude
 
type Simp = Int
 
type Simp2 = Simp
 
type OptionInt = Maybe Int
 
type Qual4 = Test.Deep.Qual3 Bool
 
data Either_ a b = Left_ a Int
                 | Right_ Int b
                 deriving Eq
left (Left_ a _) = a
right (Right_ _ a) = a
 
data List a = Nil
            | Cons a (List a)
            deriving Eq
 
data ListExtra a b = NilE
                   | ConsE a (ListExtra a b)
                   deriving Eq
 
data Qual a b = Con1 (Test.Qual1 a)
              | Con2 (Test.Deep.Qual2 b)
              deriving Eq