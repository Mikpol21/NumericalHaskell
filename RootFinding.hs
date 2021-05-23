import Functions
import Numeric.LinearAlgebra


------- Root finding -------


tolerance :: Double
tolerance = 10**(-8)

done :: R -> Bool
done x = abs x <= tolerance

newtonMethod :: Minimizer R
newtonMethod unif x
    | done (f x)    = x
    | otherwise = newtonMethod unif (x - f x/ df x) 
    where   f = func unif
            df = derivative unif

secantMethod :: R -> Minimizer R
secantMethod  x2 unif x1
    | done (f x1)    = x1
    | otherwise = secantMethod x1 unif newX
    where   f = func unif
            newX = x1 - f x1 * (x1 - x2)/(f x1 - f x2)

