{-#LANGUAGE FlexibleContexts#-}
module WeightDecay(
    input,
    testdata,
    traindata,
    testY,
    testX,
    trainLinRegUnkownTarget,
    trainAndTestWithData,
    trainAndTestWithRegularization,
    makeTransformedMatrix,
    listToTuple'
)
where

import Network.HTTP
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix as HMatrix
import NonlinearTransform
import LinearRegression
import Data.Maybe
--------------------------------------------------------------
-- Solutions to homework 6 of "Learning from data" question 5-6
--------------------------------------------------------------
input http = do
  response <- simpleHTTP $ getRequest http
  let body = fmap rspBody response
  let rows = fmap  ( map (map readDouble . words) . lines)  body
  return rows

readDouble :: String -> Double
readDouble = read

listToTuple' :: [a] -> Maybe (a,a)
listToTuple' (x:y:xs) = Just (x,y)
listToTuple' _ = Nothing

-- | downloads the test data
testdata = input "http://work.caltech.edu/data/out.dta"

-- | downloads the training data
traindata = input "http://work.caltech.edu/data/in.dta"
  
testY = fmap ( fmap makeVectorY )testdata
    where makeVectorY lists = vector $ map last lists

testX = makeDataMatrix testdata

trainY = fmap ( fmap makeVectorY ) traindata
    where makeVectorY lists = vector $ map last lists

trainX = makeDataMatrix traindata

makeLabelVector inputdata = fmap ( fmap makeVectorY ) inputdata
    where makeVectorY lists = vector $ map last lists

makeDataMatrix inputdata = fmap (fmap makeMatrixX) inputdata
    where makeMatrixX lists = matrix 3 $ concatMap xvectors lists
          xvectors list = 1 : init list

makeTransformedMatrix2 inputdata = fmap (fmap makeMatrixX) inputdata
    where makeMatrixX lists = createTransformedX2 $ map (fromMaybe (0,0) .listToTuple') lists

makeTransformedMatrix k inputdata = fmap (fmap makeMatrixX) inputdata
    where makeMatrixX lists = createTransformedXk k $ map (fromMaybe (0,0) .listToTuple') lists

trainXTransformed2   = makeTransformedMatrix2 traindata

testXTransformed2   = makeTransformedMatrix2 testdata



-- | Trains linear Regression from the data on the course website

trainLinRegUnkownTarget  = do    
    matrixXTrain <- trainXTransformed2
    vectorYTrain <- trainY
    let weights =  linearRegressionWeight<$>  matrixXTrain <*> vectorYTrain
    let inSampleError =  linRegClassificationError <$> matrixXTrain <*> vectorYTrain<*> weights
    return (weights,inSampleError)

-- | Tests linear Regression from the data on the course website
testLinRegUnkownTarget weights = do
    matrixXTest <- testXTransformed2
    vectorYTest <- testY
    let outOfSampleError = linRegClassificationError <$> matrixXTest <*> vectorYTest<*> weights
    return outOfSampleError

-- | solution for question 2: train and test with given data, with nonlinear transformation but without regularization and print the result
trainAndTestWithData :: IO ()
trainAndTestWithData = do
    (weights, inSampleError) <- trainLinRegUnkownTarget
    outOfSampleError <- testLinRegUnkownTarget weights
    putStr "In Sample Error: "
    print inSampleError
    putStr "Out of Sample Error: "
    print outOfSampleError

-- | returns the weights determined by linear regression on the training data with regularization
linearRegressionWeightReg :: R -> Matrix R -> Vector R  ->Vector R
linearRegressionWeightReg regParam matrixOfInputData vectorOfLabels  = inv (tr matrixOfInputData HMatrix.<> matrixOfInputData + scalar regParam * (ident $fst$size  $tr matrixOfInputData)) HMatrix.#> (tr matrixOfInputData #> vectorOfLabels)

-- | Trains linear Regression from the data on the course website with regularization 

trainLinRegWithRegularization regParam = do    
    matrixXTrain <- trainXTransformed2
    vectorYTrain <- trainY
    let weights =  linearRegressionWeightReg regParam <$>  matrixXTrain <*> vectorYTrain
    let inSampleError =  linRegClassificationError <$> matrixXTrain <*> vectorYTrain<*> weights
    return (weights,inSampleError)

-- | performs training and testing with the regularization parameter provided as an input and prints the in-sample and out-of-sample error
trainAndTestWithRegularization :: R -> IO()
trainAndTestWithRegularization regParam = do
    (weights, inSampleError) <- trainLinRegWithRegularization regParam
    outOfSampleError <- testLinRegUnkownTarget weights
    putStr "In Sample Error: "
    print inSampleError
    putStr "Out of Sample Error: "
    print outOfSampleError
