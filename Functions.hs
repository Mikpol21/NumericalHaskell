module Functions where
import Numeric.LinearAlgebra

data Function a = Multivariate {func ::     a -> R,
                                gradient :: a -> Vector R,
                                hessian ::  a -> Maybe (Matrix R)} |
                  Univariate   {func ::     a -> R,
                                derivatives::Function a} | 
                  Bottom

type Minimizer a = Function a -> a -> a
type Integration = Function R -> (R, R) -> R


-- returns df :: R -> R
derivative :: Function R -> (R -> R)
derivative = func . derivatives

-- Returns Univariate Function from list
toUniFunction :: [R -> R] -> Function R
toUniFunction = foldr Univariate Bottom

------------------------------  Example functions ------------------------------ 

easy :: Function (Vector R)
easy = Multivariate (\x -> (x - vector [1, 2, 3]) `dot` (x - vector [1, 2, 3]))
                (\x -> 2.0 * (x - vector [1, 2, 3]))
                (const Nothing)

square :: Function R
square = toUniFunction [\x -> x*x - sqrt 3, (2*)]

someExp :: Function R
someExp = toUniFunction [\x -> x * exp x - 2, \x -> exp x + x * exp x]

