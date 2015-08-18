
import Odeint
import Text.Printf
import Prelude as P
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I

type V = Vector Double

lorenz :: Acc V -> Acc V -> Acc V
lorenz mu v = stack [p*(y-x), x*(r-z)-y, x*y - b*z]
  where
    p = mu ! (index1 0)
    r = mu ! (index1 1)
    b = mu ! (index1 2)
    x = v ! (index1 0)
    y = v ! (index1 1)
    z = v ! (index1 2)

timeline :: (Acc V -> Acc V) -> Acc V -> [Acc V]
timeline f v = vn:(timeline f vn)
  where vn = use $ run $ f v

printToCSV :: [Acc V] -> IO()
printToCSV (t:tl) = do
  let (x:y:z:_) = toList $ run t
  printf "%f,%f,%f\n" x y z
  printToCSV tl
printToCSV [] = return ()

slice' :: Int -> [a] -> [a]
slice' i (x:xs) = x:(slice' i $ P.drop (i-1) xs)
slice' _ [] = []

main :: IO ()
main = do
  let arr = use $ fromList (Z:.3) [1, 0, 0] :: Acc V
  let mu = use $ fromList (Z:.3) [10, 28, 8.0/3.0] :: Acc V
  let dt = 0.01 :: Exp Double
  let f = eEuler (lorenz mu) dt
  let tl = timeline f arr :: [Acc V]
  putStrLn "x,y,z"
  printToCSV $ slice' 10 $ P.take 10000 tl
