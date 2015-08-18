
import Odeint()
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I

type V = Vector Double

stack :: [Exp Double] -> Acc V
stack (x:xs) = (A.++) v $ stack xs
  where
    v = fill (index1 1) x :: Acc V
stack [] = fill (index1 0) 0

lorenz :: Acc V -> Acc V
lorenz v = stack [y, x, z]
  where
    x = v ! (index1 0)
    y = v ! (index1 1)
    z = v ! (index1 2)

main :: IO ()
main = do
  let arr = use $ fromList (Z:.3) [1..] :: Acc V
  print $ lorenz arr
  print $ run $ lorenz arr
