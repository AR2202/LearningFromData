
{-#LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
module LogisticRegression
(y,
m,
b,
Point(..),
runReader,
epoch,
weightReader,
createRandomPoints,
trainAndTest

)
where
import Control.Monad.Reader
import Data.List
import System.Random
import Data.Traversable(for)

--------------------------------
--  Solutions to homework 5, problems 8 & 9 of
--  the 'Learning from Data' course
-- http://work.caltech.edu/homeworks.html
-------------------------------------


-------------------------------
-- Types and Typeclass Instances
--------------------------------

type Weight = (Float,Float,Float)
type Point = (Float,Float)

instance Num Weight where
    (+) (x0,x1,x2) (w0,w1,w2) = (w0+x0,  w1+x1,  w2+x2)
    (-) (x0,x1,x2)(w0,w1,w2)=(x0-w0,x1-w1,x2-w2)
    (*) (x0,x1,x2) (w0,w1,w2) = (w2*x1 - w1*x2, w2*x0 - w0*x2,w1*x0-w0*x1)
    negate (w0,w1,w2)=(negate w0,negate w1, negate w2)
    abs (w0,w1,w2) = (abs w0, abs w1, abs w2)
    fromInteger x = (fromInteger x, 0.0, 0.0)
    signum (w0,w1,w2) = (signum w0, signum w1, signum w2)
---------------------------------------------
-- Creating the line for the target function
--------------------------------------------
m :: Point -> Point -> Float
m (x1,x2)(z1,z2) = (x2 - z2)/(x1 - z1)

b :: Point -> Point -> Float
b (x1,x2) (z1,z2) = x2 -x1 *(x2-z2)/(x1-z1)

-- | Takes 2 Points and returns the function of a line connecting them
line :: Point -> Point -> (Float -> Float)
line point1 point2 = \x -> m point1 point2 * x + b point1 point2

-----------------------------------
-- functions for the Weight type
------------------------------------

-- | The dot product
dotProd :: Weight -> Weight -> Float
dotProd (x0,x1,x2) (w0,w1,w2) = w0*x0 + w1*x1 + w2*x2

-- | Scalar multiplication of the weight vecotr
prod :: Float -> Weight  -> Weight
prod yn (x0,x1,x2)  = (x0*yn,x1*yn,x2*yn)

-- | Converting a point to a weight by adding the first coordinate (1)
point2weight :: Point -> Weight
point2weight (x1,x2) =(1,x1,x2)

-- | looks up the label of the point by applying the target function
y :: (Float -> Float) -> Point -> Float
y yLine (x1,x2) = if yLine x1 < x2 then (-1) else 1

-- | The CrossEntropy error of one point
crossEntropy1 yLine w x = log (1+ exp(-1*y1 *dotProd xw w))
    where y1 = y yLine x
          xw = point2weight x

 -- | The CrossEntropy error of a list of points         
crossEntropy yLine w points = (*) 0.01 $ sum $ map (crossEntropy1 yLine w) points

-- | The gradient of the CrossEntropy with respect to the weight for one point
dCrossEntropy1 :: (Float->Float) -> Weight -> Point -> Weight
dCrossEntropy1 yLine w x = prod (-1/(1+ exp(y1* dotProd xw w)))(prod y1 xw ) 
    where y1 = y yLine x
          xw = point2weight x

-- | The update function for the weight
updateWeight yLine  eta w x =  w - prod  eta (dCrossEntropy1 yLine w x) 

-- | Performs 1 training epoch (one run through all training points)
epoch yLine eta = foldl' (updateWeight yLine eta) 

-- | stores the target function and performs training, returning the weight and the number of epochs performed before convergence
weightReader :: [Point] -> Reader (Float ->Float) (Weight,Int)
weightReader points = do
    targetFunc <-ask
    let pointpermutations = pointPermutations points
    let weightsEpochs = finalWeights 0.01 targetFunc 0.01 (0,0,0) pointpermutations 0
    return weightsEpochs

-- | performs testing and returns the CrossEntropy error
errReader :: [Point] -> Weight-> Reader (Float ->Float) Float
errReader points weights = do
    targetFunc <-ask
    let testError = crossEntropy  targetFunc weights points
    return testError

-- | performs training epochs until the stop criterion for convergence is reached
finalWeights stopcrit yLine eta weight0 pointpermutations epochs
    |vecval (weight1 -weight0)<stopcrit = (weight1,epochs)
    |otherwise = finalWeights stopcrit yLine eta weight1 (tail  pointpermutations) (epochs+1)
        where 
            weight1 = epoch yLine eta weight0 $head pointpermutations
            vecval (w0,w1,w2) = sqrt $ w0**2 + w1**2 + w2**2

-- |Creates a random Point between (-1,-1) and (1,1)
createRandomPoint = do
    rand1 :: Float <- randomRIO (-1,1)
    rand2 :: Float <- randomRIO (-1,1)
    return  (rand1,rand2)

-- |Creates a list of n random Points between (-1,-1) and (1,1)
createRandomPoints n  = do
    randList1 :: [Float] <- for [1 .. n] $ \_ -> randomRIO (-1,1)
    randList2 :: [Float] <- for [1 .. n] $ \_ -> randomRIO (-1,1)
    return $ zip randList1 randList2

 -- | creates an infinite list of permutations of the list provided as an argument  
pointPermutations = cycle . permutations 

---------------------------------------------------------------
-- The final loop that creates the target function and training and testing data,
-- performs training and testing
-------------------------------------------------------------------

-- | performs the whole training and testing loop, printing the weights, epochs and the test error
trainAndTest = do
    point1 <- createRandomPoint
    point2 <- createRandomPoint

    let target = line point1 point2
    trainpoints <- createRandomPoints 100
    let (weights,epochs) = runReader (weightReader trainpoints)  target
    putStr "Weights: "
    print weights
    putStr "Epochs: "
    print epochs
    testpoints <- createRandomPoints 100
    let testError = runReader (errReader testpoints weights)  target
    putStr "Eout: "
    print testError

