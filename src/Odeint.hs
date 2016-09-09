{-# LANGUAGE BangPatterns #-}

module Odeint
    ( eEuler
    , rEuler
    , rk4
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

rk4 :: (Shape sh)
    => (Array U sh Double -> Array U sh Double)
    -> Double
    -> Array U sh Double
    -> Array U sh Double
rk4 f dt vec = computeS $ (xpy $ dt/6.0) vec $ (xpy 2.0) (k1 +^ k4) (k2 +^ k3)
  where
    k1 = f vec
    l1 = computeS $ (xpy $ 0.5*dt) vec k1
    k2 = f l1
    l2 = computeS $ (xpy $ 0.5*dt) vec k2
    k3 = f l2
    l3 = computeS $ (xpy dt) vec k3
    k4 = f l3
