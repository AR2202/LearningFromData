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
alphasQuadProg
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

-- | solves quadratic Programming on the input data matrix and label Vector
alphasQuadProg :: Matrix R -> Vector R -> Either QuadProgPPError (Vector R, Double)
alphasQuadProg dataMatrix labelVector = solveQuadProg (matrixA,vectorB) (Just (labelVecMatrix,zeros)) Nothing
    where matrixA = makeQuadraticMatrix dataMatrix labelVector
          vectorB = vector $ replicate (size labelVector) (-1)
          labelVecMatrix = asRow labelVector
          zeros = konst 0 (size labelVector)

-- | trains a perceptron wiht initial weights obtained by linear Regression
trainPLAandSVM :: Int -> IO()
trainPLAandSVM n = do
    target<-makeTargetFunction
    trainpoints <- createRandomPoints n   
    let trainX = createMatrixX trainpoints
    let trainY = createVectorY (y target) trainpoints
    let initialWeights =  (0,0,0) 
    (finalWeights,epochs)<-trainPLAWithInitial initialWeights trainpoints target
    putStrLn "Perceptron:"
    putStr "Weights: "
    print finalWeights
    putStr "Epochs: "
    print epochs
    putStrLn "Support Vector Machine:"
