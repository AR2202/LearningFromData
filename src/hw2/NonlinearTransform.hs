module NonlinearTransform (
solution8,
solution9,
solution10,
createTransformedX2,
createTransformedX,
createTransformedXk
)
where
--------------------------------------------------------------
-- Solutions to homework 2 of "Learning from data" question 8-10
--------------------------------------------------------------

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import LinearRegression

-- | the target function
targetf :: (R,R) -> R
targetf (x1,x2) = signum(x1^2 + x2^2 - 0.6 )

-- | returns the second value in a 3-tuple
sndOfTriple :: (a,b,c) -> b
sndOfTriple (_,b,_) = b

-- | returns the first value in a 3-tuple
fstOfTriple :: (a,b,c) -> a
fstOfTriple (a,_,_) = a

-- | Creates the transformed matrix X with feature vectors (1,x1,x2,x1*x2,x1^2,x2^2) from a list of data points
createTransformedX :: Int -> ([(R,R)] -> [R])-> [(R,R)]-> Matrix R
createTransformedX numFeatures transformfunction listOfPoints = matrix numFeatures listOfNumbers
    where listOfNumbers = transformfunction listOfPoints
        
transformFunction1 = \listOfPoints -> concat [[1,a,b,a*b,a^2,b^2]| (a,b)<-listOfPoints]

transformFunction2 = \listOfPoints -> concat [[1,a,b,a*b,a^2,b^2,abs(a-b),abs(a+b)]| (a,b)<-listOfPoints]

transformFunctionk k = \listOfPoints -> concat [take (k+1) [1,a,b,a^2,b^2,a*b,abs(a-b),abs(a+b)]| (a,b)<-listOfPoints]

createTransformedX1 listOfPoints = createTransformedX 6 transformFunction1 listOfPoints

createTransformedX2 listOfPoints = createTransformedX 8 transformFunction2 listOfPoints

createTransformedXk k listOfPoints = createTransformedX (k+1) (transformFunctionk k) listOfPoints
-- | solution to Problem 8
solution8 :: IO R
solution8 = fmap sndOfTriple $ trainLinRegWNoise targetf 1000 0.1 createMatrixX

-- | solution to Problem 9
solution9 :: IO (Vector R)
solution9 = fmap fstOfTriple $ trainLinRegWNoise targetf 1000 0.1 createTransformedX1

-- | solution to Problem 10
solution10 :: IO R
solution10 = solution9 >>= testLinRegWNoise targetf 1000 0.1 createTransformedX1



