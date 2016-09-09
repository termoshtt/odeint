{-# LANGUAGE BangPatterns #-}

module Odeint
    ( eEuler
    , rEuler
    ) where

import Data.Array.Repa as Repa


xpy p = Repa.zipWith (\ x y -> x + p*y)

eEuler :: (Shape sh)
       => (Array U sh Double -> Array U sh Double)
       -> Double
       -> Array U sh Double
       -> Array U sh Double
eEuler f dt vec = computeS $ (xpy dt) vec (f vec)

rEuler :: (Shape sh)
       => (Array U sh Double -> Array U sh Double)
       -> Double
       -> Array U sh Double
       -> Array U sh Double
rEuler f dt vec = computeS $ Repa.zipWith (+) l1 k2
  where
    k1 = f vec
    l1 = computeS $ (xpy dt) vec k1
    k2 = f l1
