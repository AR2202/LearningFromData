{-#LANGUAGE FlexibleContexts#-}
module SVM (
multiplyByItself,
testmatrix,
testvector,
replicateVector,
replicateVectorSquared,
makeLabelAgreementMatrix
)
where

import PLA
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix as HMatrix

----------------------------------------------------
-- Solutions to Homework 7 on SVM
----------------------------------------------------

-- | a test matrix - not part of homework
testmatrix :: Matrix R
testmatrix = matrix 2 [1,1,1,-1,-1,-1,3,4,5,6]

-- | a test vector - not part of homework
testvector :: Vector R
testvector = vector [-1,1,1,-1,1]

-- | create a matrix where every entry x_n_m is the dot product of the nth vector with the mth vector
multiplyByItself :: Matrix R -> Matrix R
multiplyByItself matrixA = matrixA HMatrix.<> (tr matrixA)

-- | make a matrix where every entry corresponds to the product of the labels of the corresponding data _ it will be 1 if they agree and -1 otherwise
makeLabelAgreementMatrix :: Vector R -> Matrix R
makeLabelAgreementMatrix labelvector = diag labelvector HMatrix.<>  replicateVectorSquared labelvector

-- | make a matrix form a vector by repeating the vector n times and arranging into n rows, each corresponding to the vector
replicateVector :: Int -> Vector R -> Matrix R
replicateVector n v = fromRows $ replicate n v

-- | make a squared matrix from a vector, such that each rows corresponds to the vector
replicateVectorSquared v = replicateVector (size v ) v


