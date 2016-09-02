
import Odeint
import Numeric.LinearAlgebra
import Criterion.Main

type V = Vector Double

lorenz :: V -> V -> V
lorenz mu v = vector [p*(y-x), x*(r-z)-y, x*y - b*z]
  where
    p = mu ! 0
    r = mu ! 1
    b = mu ! 2
    x = v ! 0
    y = v ! 1
    z = v ! 2

timeline :: (V -> V) -> V -> [V]
timeline teo x0 = x1:timeline teo x1
  where
    x1 = teo x0

teo :: V -> Int -> V
teo v n = head $ drop n $ timeline l v
  where
    l = lorenz $ vector [10, 28, 8.0/3.0]

main = defaultMain [
  bgroup "Lorenz" [ bench "100000" $ nf v 100000
                  , bench "1000000" $ nf v 1000000
                  ]
  ]
  where
    v = teo (vector [1, 0, 0]) :: Int -> V
