
import System.Random
import Data.List
import Functions
import Optimization
import Numeric.LinearAlgebra hiding (magnitude)

main = do
    seed  <- newStdGen
    let rs = randomVector 10 Uniform 200
    let function = randomQuadratic 200 rs
    print $ magnitude ( rs - gradientDescent function (vector [0..199]))
    print $ magnitude ( rs - conjugateDescent function (vector [0..199]))
    print $ magnitude ( rs - newtonsMethod function (vector [0..199]))



------------------------------  Example functions ------------------------------ 
randomQuadratic :: Int -> Vector R -> Function (Vector R)
randomQuadratic n r = Multivariate(\x -> (x - r) `dot` (x -  r))
                                (\x -> 2.0 * (x - r))
                                (\x -> Just (2.0 * identity n))



easy :: Function (Vector R)
easy = Multivariate (\x -> (x - vector [1, 2, 3]) `dot` (x - vector [1, 2, 3]))
                (\x -> 2.0 * (x - vector [1, 2, 3]))
                (const Nothing)

square :: Function R
square = toUniFunction [\x -> x*x - sqrt 3, (2*)]

someExp :: Function R
someExp = toUniFunction [\x -> x * exp x - 2, \x -> exp x + x * exp x]

