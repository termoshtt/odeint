{-# LANGUAGE BangPatterns #-}

module Numeric.Odeint.Examples
    ( lorenz63
    ) where

import Data.Array.Repa as Repa
import Numeric.Odeint

lorenz63 :: (Double, Double, Double) -> DArray DIM1 -> DArray DIM1
lorenz63 (p, r, b) v = fromListUnboxed (Z :. 3) [p*(y-x), x*(r-z)-y, x*y - b*z]
  where
    x = v ! (Z :. 0)
    y = v ! (Z :. 1)
    z = v ! (Z :. 2)
