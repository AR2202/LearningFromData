
{-#LANGUAGE FlexibleContexts#-}
module LinearRegression
(trainLinReg,
avError

)
where

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import System.Random
import Data.Traversable (for)
import Control.Monad(replicateM)

--------------------------------------------------------------
-- Solutions to homework 2 of "Learning from data" question 5-6
--------------------------------------------------------------

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

-- | Squared error
squaredError:: Matrix R -> Vector R -> Vector R -> R
squaredError matrixOfInputData vectorOfLabels weight = 1/fromIntegral n * dot v v
    where v = signum (matrixOfInputData #> weight) - vectorOfLabels
          n = size vectorOfLabels

-- | Classification error for linear Regression (i.e. fraction of misclassified points)
linRegClassificationError matrixOfInputData vectorOfLabels weight = numberMisclassified/totalDatapoints 
    where numberMisclassified = norm_0 $ signum (matrixOfInputData #> weight) - vectorOfLabels
          totalDatapoints = fromIntegral $ size vectorOfLabels

----------------------------------------------
-- training and testing the Linear Regression model

-- | training linear Regression on random dataset of 100 points and testing on 1000 points and returning a pair of in-Sample and out-of-sample error
trainLinReg :: IO (R,R)
trainLinReg = do
    point1 <- createRandomPoint
    point2 <- createRandomPoint
    let target = line point1 point2
    trainpoints <- createRandomPoints 100
    testpoints <- createRandomPoints 1000
    let trainX = createMatrixX trainpoints
    let trainY = createVectorY target trainpoints
    let weights = linearRegressionWeight trainX trainY
    let testX = createMatrixX testpoints
    let testY = createVectorY target testpoints

    let inSampleError = linRegClassificationError trainX trainY weights
    let outOfSampleError = linRegClassificationError testX testY weights
    return (inSampleError, outOfSampleError)
---------------------------------------------------------
-- averaging several runs of the algorithm
---------------------------------------------------------

-- | average in a list of numbers
average :: [R] -> R
average xs = sum xs / fromIntegral (length xs)

-- | average of in-sample and out-of-sample error, calculated from a list of pairs and returned as a pair.
averageErrors :: [(R,R)]->(R,R)
averageErrors pairs = (avFst,avSnd)
    where avFst = average $ map fst  pairs
          avSnd = average $ map snd  pairs

-- |pair of average in-sample and out-of-sample error in n runs of linear Regression on n different sets of points
avError :: Int -> IO ()
avError n = putStrLn .( "In-sample error, out-of-sample error: " ++).show =<< fmap averageErrors (replicateM n trainLinReg)