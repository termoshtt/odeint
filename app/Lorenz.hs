
import Odeint
import Numeric.LinearAlgebra

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

main :: IO ()
main = do
  let mu = vector [10, 28, 8.0/3.0]
  let teo = eEuler (lorenz mu) 0.01
  let v = vector [1, 0, 0]
  print $ teo v
