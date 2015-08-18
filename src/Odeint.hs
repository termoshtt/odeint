{-# LANGUAGE BangPatterns #-}

module Odeint
    ( eEuler
    , rEuler
    , stack
    ) where

import Data.Array.Accelerate as A

type V = Vector Double

plus :: Acc V -> Acc V -> Acc V
plus x y = A.zipWith (+) x y

mult :: Exp Double -> Acc V -> Acc V
mult a x = A.map (*a) x

eEuler :: (Acc V -> Acc V) -> Exp Double -> Acc V -> Acc V
eEuler f dt !vec = vec `plus` dx
  where
    dx = dt `mult` f vec

rEuler :: (Acc V -> Acc V) -> Exp Double -> Acc V -> Acc V
rEuler f dt !vec = vec `plus` k1 `plus` k2
  where
    k1 = (dt / 2) `mult` f vec
    k2 = (dt / 2) `mult` f (vec `plus` k1)

stack :: [Exp Double] -> Acc V
stack (x:xs) = (A.++) v $ stack xs
  where
    v = fill (index1 1) x :: Acc V
stack [] = fill (index1 0) 0
