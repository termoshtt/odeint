{-# LANGUAGE BangPatterns #-}

module Odeint
    ( eEuler
    ) where

import Data.Array.Repa as Repa

eEuler :: (Num a, Shape sh, Source r1 a, Source r2 a)
       => (Array r1 sh a -> Array r2 sh a)
       -> a
       -> Array r1 sh a
       -> Array D sh a
eEuler f dt vec = Repa.zipWith (\ v x -> v + dt * x) vec dx
  where
    !dx = f vec
