module NonlinearTransform (
solution8,
solution9,
solution10
)
where

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import LinearRegression
targetf (x1,x2) = signum(x1^2 + x2^2 - 0.6 )
sndOfTriple (_,b,_)=b
fstOfTriple (a,_,_)=a
solution8 = fmap sndOfTriple $ trainLinRegWNoise targetf 1000 0.1 createMatrixX

solution9 = fmap fstOfTriple $ trainLinRegWNoiseTransformed targetf 1000 0.1

solution10 = solution9 >>= testLinRegWNoiseTransformed targetf 1000 0.1

-- | Creates the transformed matrix X with feature vectors (1,x1,x2,x1*x2,x1^2,x2^2) from a list of data points
createTransformedX :: [(R,R)] -> Matrix R
createTransformedX listOfPoints = matrix 6 listOfNumbers
    where listOfNumbers = concat [[1,a,b,a*b,a^2,b^2]| (a,b)<-listOfPoints]

-- | Train the Linear Regression model with the transformed features
trainLinRegWNoiseTransformed :: ((R,R) -> R) -> Int -> Float -> IO (Vector R, R, [(R, R)])
trainLinRegWNoiseTransformed target n noiselevel = do    
    trainpoints <- createRandomPoints n
    
    let trainX = createTransformedX trainpoints
    let trainY = createVectorY target trainpoints
    trainYNoisy <- flipLabels noiselevel trainY
    let weights = linearRegressionWeight trainX trainYNoisy
    let inSampleError = linRegClassificationError trainX trainYNoisy weights
    return (weights,inSampleError,trainpoints)

testLinRegWNoiseTransformed target n noiselevel weights  = do
    testpoints <- createRandomPoints n
    let testX = createTransformedX testpoints
    let testY = createVectorY target testpoints
    testYNoisy <- flipLabels noiselevel testY    
    let outOfSampleError = linRegClassificationError testX testYNoisy weights
    return  outOfSampleError

