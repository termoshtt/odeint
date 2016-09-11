
import Numeric.Odeint
import Numeric.Odeint.Examples
import Data.Array.Repa as Repa
import Criterion.Main

main :: IO ()
main = defaultMain [
  bgroup "Lorenz63" [ bench "10k steps" $ whnf takeNth 10000
                    , bench "100k steps" $ whnf takeNth 100000
                    ]
  ]
  where
    teo = lorenz63 (10, 28, 8.0/3.0)
    v0 = fromListUnboxed (Z :. 3) [1, 0, 0]
    takeNth = \ n -> (iterate teo v0) !! n
