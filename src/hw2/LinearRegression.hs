{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearRegression
  ( trainLinReg
  , trainAndTestLinReg
  , trainLinRegWNoise
  , avError
  , linRegAsInitialForPLA
  , createVectorY
  , createRandomPoints
  , flipLabels
  , linearRegressionWeight
  , linRegClassificationError
  , testLinRegWNoise
  , createMatrixX
  , makeTargetFunction
  , y
  ) where

import           Control.Monad                 (replicateM)
import           Data.Maybe
import           Data.Traversable              (for)
import           Numeric.LinearAlgebra.Data
import           Numeric.LinearAlgebra.HMatrix
import           PLA
import           System.Random

--------------------------------------------------------------
-- Solutions to homework 2 of "Learning from data" question 5-6
--------------------------------------------------------------
---------------------------------------------
-- Creating the line for the target function
--------------------------------------------
-- | takes 2 points and returns the slope of the line connecting them
m :: (R, R) -> (R, R) -> R
m (x1, x2) (z1, z2) = (x2 - z2) / (x1 - z1)

-- | takes 2 points and returns the y-intercept of the line connecting them
b :: (R, R) -> (R, R) -> R
b (x1, x2) (z1, z2) = x2 - x1 * (x2 - z2) / (x1 - z1)

-- | Takes 2 Points and returns the function of a line connecting them
line point1 point2 = \x -> m point1 point2 * x + b point1 point2

-----------------------------------
-- Creating random data points
------------------------------------
-- |Creates a random Point between (-1,-1) and (1,1)
createRandomPoint :: IO (R, R)
createRandomPoint = do
  rand1 <- randomRIO (-1, 1)
  rand2 <- randomRIO (-1, 1)
  return (rand1, rand2)

-- |Creates a list of n random Points between (-1,-1) and (1,1)
createRandomPoints :: Int -> IO [(R, R)]
createRandomPoints n = do
  randList1 <- for [1 .. n] $ \_ -> randomRIO (-1, 1)
  randList2 <- for [1 .. n] $ \_ -> randomRIO (-1, 1)
  return $ zip randList1 randList2

-----------------------------------------------------
-- Preparing data for processing by Linear Regression
------------------------------------------------------
-- | looks up the label of the point by applying the target function
y :: (R -> R) -> (R, R) -> R
y yLine (x1, x2) =
  if yLine x1 < x2
    then (-1)
    else 1

-- | Creates the vector of labels using the target function 'yLine' and a list of data points
createVectorY :: ((R, R) -> R) -> [(R, R)] -> Vector R
createVectorY target listOfPoints = vector $ map target listOfPoints

-- | Creates the matrix X from a list of data points
createMatrixX :: [(R, R)] -> Matrix R
createMatrixX listOfPoints = matrix 3 listOfNumbers
  where
    listOfNumbers = concat [[1, a, b] | (a, b) <- listOfPoints]

flipLabels :: Float -> Vector R -> IO (Vector R)
flipLabels noiselevel labelvector = do
  gen <- getStdGen
  let numLabels = size labelvector
  let randnums :: [Int] = randomRs (1, numLabels) gen
  let toFlip = take (floor (noiselevel * fromIntegral numLabels)) randnums
  let flipVector =
        vector
          [ if x `elem` toFlip
            then (-1)
            else 1
          | x <- [1 .. numLabels]
          ]
  let flippedLabels = labelvector * flipVector
  return flippedLabels

--------------------------------------------------------
-- Linear Regression algorithm
--------------------------------------------------------
-- | returns the weights determined by linear regression on the training data
linearRegressionWeight :: Matrix R -> Vector R -> Vector R
linearRegressionWeight matrixOfInputData vectorOfLabels =
  pinv matrixOfInputData #> vectorOfLabels

-- | Squared error
squaredError :: Matrix R -> Vector R -> Vector R -> R
squaredError matrixOfInputData vectorOfLabels weight =
  1 / fromIntegral n * dot v v
  where
    v = signum (matrixOfInputData #> weight) - vectorOfLabels
    n = size vectorOfLabels

-- | Classification error for linear Regression (i.e. fraction of misclassified points)
linRegClassificationError :: Matrix R -> Vector R -> Vector R -> R
linRegClassificationError matrixOfInputData vectorOfLabels weight =
  numberMisclassified / totalDatapoints
  where
    numberMisclassified =
      norm_0 $ signum (matrixOfInputData #> weight) - vectorOfLabels
    totalDatapoints = fromIntegral $ size vectorOfLabels

--------------------------------------------------
-- training and testing the Linear Regression model
--------------------------------------------------
-- | create the target function
makeTargetFunction :: IO (R -> R)
makeTargetFunction = do
  point1 <- createRandomPoint
  point2 <- createRandomPoint
  let target = line point1 point2
  return target

-- | training linear Regression on random dataset of n noisy points and returning (weights, in-Sample error, trainingpoints)
trainLinRegWNoise ::
     ((R, R) -> R)
  -> Int
  -> Float
  -> ([(R, R)] -> Matrix R)
  -> IO (Vector R, R, [(R, R)])
trainLinRegWNoise target n noiselevel transformFunction = do
  trainpoints <- createRandomPoints n
  let trainX = transformFunction trainpoints
  let trainY = createVectorY target trainpoints
  trainYNoisy <- flipLabels noiselevel trainY
  let weights = linearRegressionWeight trainX trainYNoisy
  let inSampleError = linRegClassificationError trainX trainYNoisy weights
  return (weights, inSampleError, trainpoints)

-- | training linear Regression on random dataset of n noiseless points and returning (weights, in-Sample error, trainingpoints)
trainLinReg ::
     (R -> R) -> Int -> ([(R, R)] -> Matrix R) -> IO (Vector R, R, [(R, R)])
trainLinReg target n transformFunction =
  trainLinRegWNoise (y target) n 0 transformFunction

-- | evaluates linear Regression result on test data
testLinReg :: (R -> R) -> Int -> ([(R, R)] -> Matrix R) -> Vector R -> IO R
testLinReg target n transformFunction weights =
  testLinRegWNoise (y target) n 0 transformFunction weights

-- | training linear Regression on random dataset of n noisy points and returning (weights, in-Sample error, trainingpoints)
testLinRegWNoise ::
     ((R, R) -> R) -> Int -> Float -> ([(R, R)] -> Matrix R) -> Vector R -> IO R
testLinRegWNoise target n noiselevel transformFunction weights = do
  testpoints <- createRandomPoints n
  let testX = transformFunction testpoints
  let testY = createVectorY target testpoints
  testYNoisy <- flipLabels noiselevel testY
  let outOfSampleError = linRegClassificationError testX testYNoisy weights
  return outOfSampleError

-- | performs training and testing
trainAndTestLinReg :: IO (R, R)
trainAndTestLinReg = do
  target <- makeTargetFunction
  (weights, inSampleError, _) <- trainLinReg target 100 createMatrixX
  outOfSampleError <- testLinReg target 1000 createMatrixX weights
  return (inSampleError, outOfSampleError)

---------------------------------------------------------
-- averaging several runs of the algorithm
---------------------------------------------------------
-- | average in a list of numbers
average :: [R] -> R
average xs = sum xs / fromIntegral (length xs)

-- | average of in-sample and out-of-sample error, calculated from a list of pairs and returned as a pair.
averageErrors :: [(R, R)] -> (R, R)
averageErrors pairs = (avFst, avSnd)
  where
    avFst = average $ map fst pairs
    avSnd = average $ map snd pairs

-- |pair of average in-sample and out-of-sample error in n runs of linear Regression on n different sets of points
avError :: Int -> IO ()
avError n =
  putStrLn . ("In-sample error, out-of-sample error: " ++) . show =<<
  fmap averageErrors (replicateM n trainAndTestLinReg)

----------------------------------------------
-- Using linear regression to obtain initial weights for perceptron
--------------------------------------------------
listToTuple :: [a] -> Maybe (a, a, a)
listToTuple (x:y:z:xs) = Just (x, y, z)
listToTuple _          = Nothing

-- | trains a perceptron wiht initial weights obtained by linear Regression
linRegAsInitialForPLA :: IO ()
linRegAsInitialForPLA = do
  target <- makeTargetFunction
  (initialweightsVec, _, trainPoints) <- trainLinReg target 10 createMatrixX
  let maybeInitialWeights = listToTuple $ toList initialweightsVec
  let initialWeights = fromMaybe (0, 0, 0) maybeInitialWeights
  (finalWeights, epochs) <-
    trainPLAWithInitial initialWeights trainPoints target
  putStr "Weights: "
  print finalWeights
  putStr "Epochs: "
  print epochs
