
import Numeric.Odeint
import Numeric.Odeint.Examples
import Data.Array.Repa as Repa
import Criterion.Main


takeNth :: Int -> Double
takeNth n = ((iterate teo v0) !! n) ! (Z :. 0)
  where
    teo = lorenz63 (10, 28, 8.0/3.0)
    v0 = fromListUnboxed (Z :. 3) [1, 0, 0]

main :: IO ()
main = defaultMain [
  bgroup "Lorenz63" [ bench "steps=10k" $ whnf takeNth 10000
                    , bench "steps=100k" $ whnf takeNth 100000
                    ]
  ]
