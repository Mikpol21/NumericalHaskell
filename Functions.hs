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


identity :: Int ->  Matrix R
identity n = (n><n) $ concatMap (map (\x -> if x then 1.0 else 0.0)) [[x == y | x <- [1..n]] | y <- [1..n]]


