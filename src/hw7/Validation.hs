module Validation(
trainValAndTest,
valTrainAndTest,
solution1_2,
solution3_4
)
where

import WeightDecay
import NonlinearTransform
import LinearRegression
import Data.Maybe (fromMaybe)
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix as HMatrix

-- | Make a nonlinear Transformation of the first n data points of the input data
makeTransformedFirstN k n inputdata = fmap (fmap makeMatrixX ) inputdata
    where makeMatrixX lists = createTransformedXk k $ map (fromMaybe (0,0) .listToTuple')$ take n lists

-- | Make a nonlinear Transformation of the rest of the data points after the first n
makeTransformedLastN k n inputdata = fmap (fmap makeMatrixX ) inputdata
    where makeMatrixX lists = createTransformedXk k $ map (fromMaybe (0,0) .listToTuple')$ drop n lists

trainXFirstn k n   = makeTransformedFirstN k n traindata

trainXLastn k n    = makeTransformedLastN k n traindata

testXTransformedk k = makeTransformedMatrix k testdata

trainYFirstn n = fmap ( fmap makeVectorY ) traindata
    where makeVectorY lists = vector $ take n $ map last lists

trainYLastn n = fmap ( fmap makeVectorY ) traindata
    where makeVectorY lists = vector $ drop n $ map last lists

-- | Trains and validate linear Regression from the data on the course website, using the first n points for training and the rest for validation, performing non-linear transformation with k-dimensional features
trainVal k n = do    
    matrixXTrain <- trainXFirstn k n
    matrixXVal <- trainXLastn k n
    vectorYTrain <- trainYFirstn n
    vectorYVal  <- trainYLastn n
    let weights =  linearRegressionWeight<$>  matrixXTrain <*> vectorYTrain
    let inSampleError =  linRegClassificationError <$> matrixXTrain <*> vectorYTrain<*> weights
    let validationError = linRegClassificationError <$> matrixXVal <*> vectorYVal<*> weights
    return (weights,validationError,inSampleError)

-- | same as trainVal, but the first n points are used for validation and the rest for training
valTrain k n = do    
    matrixXVal <- trainXFirstn k n
    matrixXTrain <- trainXLastn k n
    vectorYVal <- trainYFirstn n
    vectorYTrain  <- trainYLastn n
    let weights =  linearRegressionWeight<$>  matrixXTrain <*> vectorYTrain
    let inSampleError =  linRegClassificationError <$> matrixXTrain <*> vectorYTrain<*> weights
    let validationError = linRegClassificationError <$> matrixXVal <*> vectorYVal<*> weights
    return (weights,validationError,inSampleError)

-- | Tests linear Regression from the data on the course website
testLinRegTransformedk k weights = do
    matrixXTest <- testXTransformedk k
    vectorYTest <- testY
    let outOfSampleError = linRegClassificationError <$> matrixXTest <*> vectorYTest<*> weights
    return outOfSampleError

-- | train, validate and test with given data, with nonlinear transformation to k-dimensional feature space, with n triaining points and max-n validation points
trainValAndTest :: Int -> Int -> IO ()
trainValAndTest k n = do
    (weights,validationError,inSampleError) <- trainVal k n
    outOfSampleError <- testLinRegTransformedk k weights
    putStr "In Sample Error: "
    print inSampleError
    putStr "Validation Error: "
    print validationError
    putStr "Out of Sample Error: "
    print outOfSampleError

-- | same as trainValAndTest, but the first n points are used for validation and the rest for training
valTrainAndTest :: Int -> Int -> IO ()
valTrainAndTest k n = do
    (weights,validationError,inSampleError) <- valTrain k n
    outOfSampleError <- testLinRegTransformedk k weights
    putStr "In Sample Error: "
    print inSampleError
    putStr "Validation Error: "
    print validationError
    putStr "Out of Sample Error: "
    print outOfSampleError

-- | solution to problems 1 and 2
solution1_2 :: IO ()
solution1_2 = mapM_ ( \k -> do
    putStr " k ="
    print k
    trainValAndTest k 25)
     [3,4,5,6,7]

-- | solution to problems 3 and 4
solution3_4 :: IO ()
solution3_4 = mapM_ ( \k -> do
    putStr " k ="
    print k
    valTrainAndTest k 25)
     [3,4,5,6,7]
