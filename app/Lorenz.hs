
import Odeint
import Numeric.LinearAlgebra
import Text.Printf

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

printCSV :: [V] -> IO ()
printCSV (v:vs) = printf "%f %f %f\n" x y z >> printCSV vs
  where
    x = v ! 0
    y = v ! 1
    z = v ! 2
printCSV [] = return ()

main :: IO ()
main = do
  let mu = vector [10, 28, 8.0/3.0]
  let teo = rk4 (lorenz mu) 0.01
  let v = vector [1, 0, 0]
  printCSV $ take 10000 $ timeline teo v
