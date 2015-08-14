{-# LANGUAGE BangPatterns #-}

module Odeint
    ( eEuler
    , rEuler
    ) where

import Data.Array.Accelerate as A

type DVector = Vector Double

plus :: Acc DVector -> Acc DVector -> Acc DVector
plus x y = A.zipWith (+) x y

mult :: Exp Double -> Acc DVector -> Acc DVector
mult a x = A.map (*a) x

eEuler :: (Acc DVector -> Acc DVector)
       -> Exp Double
       -> Acc DVector
       -> Acc DVector
eEuler f dt !vec = vec `plus` dx
  where
    dx = dt `mult` f vec

rEuler :: (Acc DVector -> Acc DVector)
       -> Exp Double
       -> Acc DVector
       -> Acc DVector
rEuler f dt !vec = vec `plus` k1 `plus` k2
  where
    k1 = (dt / 2) `mult` f vec
    k2 = (dt / 2) `mult` f (vec `plus` k1)
