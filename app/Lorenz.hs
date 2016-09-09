{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import Odeint
import Data.Array.Repa as Repa
import Criterion.Main

data Param = Param { p :: Double, b :: Double, r :: Double } deriving (Show)

type V = Array U DIM1 Double

timeline :: (V -> V) -> V -> [V]
timeline teo v = vn:timeline teo vn
  where
    vn = teo v

lorenz :: Param -> V -> V
lorenz mu v = fromListUnboxed (Z :. 3) [p'*(y-x), x*(r'-z)-y, x*y - b'*z]
  where
    p' = p mu
    r' = r mu
    b' = b mu
    x = v ! (Z :. 0)
    y = v ! (Z :. 1)
    z = v ! (Z :. 2)

takeN :: Param -> V -> Int -> V
takeN mu v n = head $ drop n $ timeline (lorenz mu) v

main :: IO ()
main = defaultMain [
  bgroup "Lorenz" [ bench "1k" $ whnf taken 1000
                  , bench "10k" $ whnf taken 10000
                  , bench "100k" $ whnf taken 100000
                  , bench "1M" $ whnf taken 1000000
                  ]
  ]
  where
    mu = Param { p = 10, r = 28, b = 8.0/3.0 }
    v0 = fromListUnboxed (Z :. 3) [1, 0, 0]
    taken = takeN mu v0
