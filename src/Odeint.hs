
module Odeint
    ( eEuler
    ) where

import Numeric.LinearAlgebra

type V = Vector Double

eEuler :: (V -> V) -> Double -> V -> V
eEuler f dt vec = vec + dx
  where
    dx = (f vec) * scalar dt :: V
