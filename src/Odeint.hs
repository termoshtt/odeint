
module Odeint
    ( eEuler
    , rEuler
    , rk4
    ) where

import Numeric.LinearAlgebra

type V = Vector Double

eEuler :: (V -> V) -> Double -> V -> V
eEuler f dt vec = vec + dx
  where
    dx = (f vec) * scalar dt

rEuler :: (V -> V) -> Double -> V -> V
rEuler f dt vec = l1 + k2
  where
    k1 = f vec * scalar (dt / 2)
    k2 = f l1 * scalar (dt / 2)
    l1 = vec + k1

rk4 :: (V -> V) -> Double -> V -> V
rk4 f dt vec = vec + (k1 + (k2 + k3) * scalar 2 + k4) * scalar (dt / 6)
  where
    k1 = f vec
    k2 = f $ vec + k1 * scalar (dt / 2)
    k3 = f $ vec + k2 * scalar (dt / 2)
    k4 = f $ vec + k3 * scalar dt
