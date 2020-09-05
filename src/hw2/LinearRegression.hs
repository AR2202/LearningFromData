module LinearRegression
(trainLinReg,
createVectorY,
createMatrixX

)
where

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import System.Random
import Data.Traversable (for)



---------------------------------------------
-- Creating the line for the target function
--------------------------------------------
m :: (R,R) -> (R,R) -> R
m (x1,x2)(z1,z2) = (x2 - z2)/(x1 - z1)

b :: (R,R) -> (R,R) -> R
b (x1,x2) (z1,z2) = x2 -x1 *(x2-z2)/(x1-z1)

-- | Takes 2 Points and returns the function of a line connecting them
line point1 point2 = \x -> m point1 point2 * x + b point1 point2
-----------------------------------
-- Creating random data points
------------------------------------

-- |Creates a random Point between (-1,-1) and (1,1)
createRandomPoint :: IO (R ,R)
createRandomPoint = do
    rand1  <- randomRIO (-1,1)
    rand2  <- randomRIO (-1,1)
    return  (rand1,rand2)

-- |Creates a list of n random Points between (-1,-1) and (1,1)
createRandomPoints :: Int -> IO [(R, R)]
createRandomPoints n  = do
    randList1  <- for [1 .. n] $ \_ -> randomRIO (-1,1)
    randList2  <- for [1 .. n] $ \_ -> randomRIO (-1,1)
    return $ zip randList1 randList2

-----------------------------------------------------
-- Preparing data for processing by Linear Regression
------------------------------------------------------

-- | looks up the label of the point by applying the target function
y :: (R -> R) -> (R,R) -> R
y yLine (x1,x2) = if yLine x1 < x2 then (-1) else 1

-- | Creates the vector of labels using the target function 'yLine' and a list of data points
createVectorY :: (R -> R) -> [(R,R)] -> Vector R
createVectorY yLine listOfPoints = vector $ map (y yLine) listOfPoints

-- | Creates the matrix X from a list of data points
createMatrixX :: [(R,R)] -> Matrix R
createMatrixX listOfPoints = matrix 3 listOfNumbers
    where listOfNumbers = concat [[1,a,b]| (a,b)<-listOfPoints]

--------------------------------------------------------
-- Linear Regression algorithm
--------------------------------------------------------

linearRegressionWeight matrixOfInputData vectorOfLabels = pinv matrixOfInputData #> vectorOfLabels


trainLinReg :: IO ()
trainLinReg = do
    point1 <- createRandomPoint
    point2 <- createRandomPoint
    let target = line point1 point2
    trainpoints <- createRandomPoints 100
    let matrixX = createMatrixX trainpoints
    let vectorY = createVectorY target trainpoints
    let weights = linearRegressionWeight matrixX vectorY
    putStr "Weights: "
    print weights