{-#LANGUAGE FlexibleContexts#-}
module SVM (
multiplyByItself,
testmatrix,
testvector,
replicateVector,
replicateVectorSquared,
makeLabelAgreementMatrix,
makeQuadraticMatrix,
testmatrixsize,
alphasQuadProg,
trainPLAandSVM
)
where

import PLA
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix as HMatrix
import Numeric.Minimization.QuadProgPP
import LinearRegression (makeTargetFunction, createMatrixX,createVectorY,createRandomPoints,y)

----------------------------------------------------
-- Solutions to Homework 7 on SVM
----------------------------------------------------

-- | a test matrix - not part of homework
testmatrix :: Matrix R
testmatrix = matrix 2 [1,1,1,-1,-1,-1,3,4,5,6]

testmatrixsize = size testmatrix

-- | a test vector - not part of homework
testvector :: Vector R
testvector = vector [-1,1,1,-1,1]

-- | create a matrix where every entry x_n_m is the dot product of the nth vector with the mth vector
multiplyByItself :: Matrix R -> Matrix R
multiplyByItself matrixA = matrixA HMatrix.<> (tr matrixA)

-- | make a matrix where every entry corresponds to the product of the labels of the corresponding data - it will be 1 if they agree and -1 otherwise
makeLabelAgreementMatrix :: Vector R -> Matrix R
makeLabelAgreementMatrix labelvector = diag labelvector HMatrix.<>  replicateVectorSquared labelvector

-- | make a matrix form a vector by repeating the vector n times and arranging into n rows, each corresponding to the vector
replicateVector :: Int -> Vector R -> Matrix R
replicateVector n v = fromRows $ replicate n v

-- | make a squared matrix from a vector, such that each rows corresponds to the vector
replicateVectorSquared :: Vector R -> Matrix R
replicateVectorSquared v = replicateVector (size v ) v

-- | makes the matrix to pass to quadratix programming
makeQuadraticMatrix :: Matrix R -> Vector R -> Matrix R
makeQuadraticMatrix dataMatrix labelVector = multiplyByItself dataMatrix * makeLabelAgreementMatrix labelVector

makeQuadraticMatrix' :: Matrix R -> Vector R -> Matrix R
makeQuadraticMatrix' dataMatrix labelVector = quadMatrix + smallValtoDiag
    where quadMatrix = makeQuadraticMatrix dataMatrix labelVector
          smallValtoDiag = diag $ konst 0.000000001 (size labelVector) 
-- | solves quadratic Programming on the input data matrix and label Vector
alphasQuadProg :: Matrix R -> Vector R -> Either QuadProgPPError (Vector R, Double)
alphasQuadProg dataMatrix labelVector = solveQuadProg (matrixA,vectorB) (Just (labelVecMatrix,zeros))(Just (idMatrix,zeros)) 
    where matrixA = makeQuadraticMatrix' dataMatrix labelVector
          vectorB = konst (-1) (size labelVector) 
          labelVecMatrix = diag labelVector
          idMatrix = ident (size labelVector) 
          zeros = konst 0 (size labelVector)

-- | Creates the data matrix from a list of data points
createDataMatrix :: [(R,R)] -> Matrix R
createDataMatrix listOfPoints = matrix 2 listOfNumbers
    where listOfNumbers = concat [[a,b]| (a,b)<-listOfPoints]

-- | trains a perceptron wiht initial weights obtained by linear Regression
trainPLAandSVM :: Int -> IO()
trainPLAandSVM n = do
    --making target function, training and testing points
    -----------------------------------------------------
    target<-makeTargetFunction
    trainpoints <- createRandomPoints n   
    testpoints <- createRandomPoints 1000
    let trainX = createDataMatrix trainpoints
    let trainY = createVectorY (y target) trainpoints
    let testX = createDataMatrix testpoints
    let testY = createVectorY (y target) testpoints
    --Perceptron
    ---------------------------------
    let initialWeights =  (0,0,0) 
    (finalWeights,epochs)<-trainPLAWithInitial initialWeights trainpoints target
    
    
    let misclassifiedTestPoints = length $ filter (isMisclassified target finalWeights) testpoints
    let prob = fromIntegral misclassifiedTestPoints/1000
    --SVM
    --------------------------
    let quadmatrix = makeQuadraticMatrix trainX trainY
    let labelVecMatrix = asColumn trainY
    let alphas = alphasQuadProg trainX trainY
    --Printing the output
    ---------------------------
    putStrLn "Perceptron:"
    putStrLn "-------------------------"
    putStr "Weights: "
    print finalWeights
    putStr "Epochs: "
    print epochs
    putStr "Probability f /= g: "
    print prob
    putStrLn "Support Vector Machine:"
    putStrLn "-------------------------"
    putStr "Alphas:"
    print alphas
