import Numeric.LinearAlgebra
import Functions


------ Integration ------

type Rule = (R -> R) -> (R, R) -> R

simpson :: Rule
simpson f (a, b) = (b - a)/6 * (f a + 4 * f m + f b)
    where m = (a + b) / 2.0

midpoint :: Rule
midpoint f (a, b) = (b - a) * f m 
    where m = (a + b) / 2.0

trapezium :: Rule
trapezium f (a, b) = (b - a)/2 * (f a + f b)

composite :: Rule -> Int -> Integration
composite rule n f (a, b) = sum $ map (rule (func f)) segments
    where   segments = [(i * len, (i + 1) * len) | i <- map toR [0..n-1]]
            len = (b - a)/toR n
            toR x = fromIntegral x :: Double

