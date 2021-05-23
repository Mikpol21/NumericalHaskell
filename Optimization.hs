import Numeric.LinearAlgebra hiding (magnitude)
import Functions
------------------------------ Optimization ------------------------------


gradientDescent :: Minimizer (Vector R)
gradientDescent = defaultdefaultMinimizer gradientMethod defaultMaxIter

conjugateDescent :: Minimizer (Vector R)
conjugateDescent = defaultdefaultMinimizer conjugateMethod defaultMaxIter

newtonsMethod :: Minimizer (Vector R)
newtonsMethod = defaultdefaultMinimizer newtonTransition defaultMaxIter


------------------------------ Gradient descent's core ------------------------------

descentKernel :: Function (Vector a) -> Params a -> NextIteration a -> StopCondition a -> Params a
descentKernel f currState next condition
    | condition currState = currState
    | otherwise = descentKernel f (next f currState) next condition

data Params a = Params {paramsX ::          Vector a,    -- x
                        paramsDirection ::  Vector a,    -- dir
                        paramsGradient ::   Vector a,    -- gradient at x
                        paramsVal ::        a,           -- f(x)
                        paramsStep ::       a,           -- step size
                        paramsIteration ::  Int          -- number of iterations
                        } deriving Show


---------- Constants ----------

armijo, ro, tolerance:: R
armijo = 10.0**(-4)
ro = 0.5
tolerance = 10 ** (-5)

defaultMaxIter :: Int
defaultMaxIter = 50


------------------------------ Stop conditions ------------------------------
type StopCondition a = (Params a -> Bool)

maxiter :: Int -> StopCondition a
maxiter maxIt (Params _ _ _ _ _ n) = n > maxIt

magnitudeOfGradient :: Vector R -> StopCondition R
magnitudeOfGradient x state = magnitude (paramsGradient state) < tolerance * (1.0 + mag x)
    where mag x = sqrt (x `dot` x)

------------------------------ Methods / Transitions ------------------------------
type NextIteration a = Function (Vector a) -> Params a -> Params a

gradientMethod :: NextIteration R
gradientMethod f currIter = Params x_next dir_next dx_next fx_next step (it + 1)
    where   Params x dir dx fx step it = currIter
            (x_next, dx_next, fx_next) = computeNextX f currIter
            dir_next = (-1.0) * dx_next

conjugateMethod :: NextIteration R
conjugateMethod f currIter = Params x_next dir_next dx_next fx_next step (it + 1)
    where   Params x dir dx fx step it = currIter
            (x_next, dx_next, fx_next) = computeNextX f currIter
            dir_next = (-1.0) * dx_next + ((dx_next - dx) `dot` dx_next / dx `dot` dx) `scale` dir

newtonTransition :: NextIteration R
newtonTransition f currIter = Params x_next dir_next dx_next fx_next step (it + 1)
    where   Params x dir dx fx step it = currIter
            (x_next, dx_next, fx_next) = computeNextX f currIter
            Just h = hessian f x
            dir_next = h <\> dx_next

------------------------------ Backtracking ------------------------------
backtracking :: NextIteration R
backtracking f currIter
    |   fx_next <= fx + armijo * step * dir `dot` dx = currIter
    |   otherwise = backtracking f $ Params x dir dx fx step_next it
    where   Params x dir dx fx step it = currIter
            (x_next, dx_next, fx_next) = computeNextX f currIter
            step_next = step * ro


stepAdjust :: Params R -> Params R -> Params R
stepAdjust prev curr = Params  x dir dx fx new_step it
    where   adjust p = paramsGradient p `dot` paramsDirection p
            new_step = step * adjust prev / adjust curr
            Params x dir dx fx step it = curr


------------------------------ Misc ------------------------------
computeNextX :: Function (Vector R) -> Params R -> (Vector R, Vector R, R)
computeNextX f (Params x dir dx fx step it) = (x_next, dx_next, fx_next)
    where   x_next = x + (step `scale` dir)
            dx_next = gradient f x_next
            fx_next = func f x_next

magnitude :: Vector R -> R
magnitude x = sqrt (x `dot` x)

getStartingParams :: Function (Vector R) -> Vector R -> Params R
getStartingParams f x = Params x dir dx fx 10.0 0
    where   dx = gradient f x
            dir = (-1.0) * dx
            fx = func f x

defaultdefaultMinimizer :: NextIteration R -> Int -> Minimizer (Vector R)
defaultdefaultMinimizer nextit n f x = paramsX $ descentKernel f start (withBacktracking nextit) stop
    where   start = getStartingParams f x
            stop = maxiter n `orWhen` magnitudeOfGradient x

withBacktracking :: NextIteration R -> NextIteration R
withBacktracking transition f params = stepAdjust params . transition f . backtracking f $ params

andWhen, orWhen :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
andWhen a b state = a state && b state
orWhen a b state = a state || b state





