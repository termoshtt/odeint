{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import Odeint
import Data.Array.Repa as Repa

data Param = Param { p :: Double, b :: Double, r :: Double } deriving (Show)

lorenz :: Param
       -> Array U DIM1 Double
       -> Array U DIM1 Double
lorenz mu v = fromListUnboxed (Z :. 3) [p'*(y-x), x*(r'-z)-y, x*y - b'*z]
  where
    p' = p mu
    r' = r mu
    b' = b mu
    x = v ! (Z :. 0)
    y = v ! (Z :. 1)
    z = v ! (Z :. 2)

main :: IO ()
main = do
  let mu = Param { p = 10, r = 28, b = 8.0/3.0 }
  let v = fromListUnboxed (Z :. 3) [1, 0, 0] :: Array U DIM1 Double
  let teo = eEuler (lorenz mu) 0.01
  let v1 = computeS $ teo v :: Array U DIM1 Double
  print v1
