
{-#LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
module LogisticRegression
(y,
m,
b,
Point(..),
createRandomPoints,
trainAndTest

)
where
import Control.Monad.Reader
import Data.List
import System.Random
import Data.Traversable(for)

--------------------------------
--  Solutions to homework 5, problems 8 & 9 of
--  the 'Learning from Data' course
-- http://work.caltech.edu/homeworks.html
-------------------------------------


-------------------------------
-- Types and Typeclass Instances
--------------------------------

type Weight = (Float,Float,Float)
type Point = (Float,Float)
type Label = Float
data LabelledPoint = LabelledPoint {pointX::Point, label::Label} 
    deriving (Show, Read, Eq)   

instance Num Weight where
    (+) (x0,x1,x2) (w0,w1,w2) = (w0+x0,  w1+x1,  w2+x2)
    (-) (x0,x1,x2)(w0,w1,w2)=(x0-w0,x1-w1,x2-w2)
    (*) (x0,x1,x2) (w0,w1,w2) = (w2*x1 - w1*x2, w2*x0 - w0*x2,w1*x0-w0*x1)
    negate (w0,w1,w2)=(negate w0,negate w1, negate w2)
    abs (w0,w1,w2) = (abs w0, abs w1, abs w2)
    fromInteger x = (fromInteger x, 0.0, 0.0)
    signum (w0,w1,w2) = (signum w0, signum w1, signum w2)
---------------------------------------------
-- Creating the line for the target function
--------------------------------------------
m :: Point -> Point -> Float
m (x1,x2)(z1,z2) = (x2 - z2)/(x1 - z1)

b :: Point -> Point -> Float
b (x1,x2) (z1,z2) = x2 -x1 *(x2-z2)/(x1-z1)

-- | Takes 2 Points and returns the function of a line connecting them
line point1 point2 = \x -> m point1 point2 * x + b point1 point2

-----------------------------------
-- functions for the Weight type
------------------------------------

-- | The dot product
dotProd :: Weight -> Weight -> Float
dotProd (x0,x1,x2) (w0,w1,w2) = w0*x0 + w1*x1 + w2*x2

-- | Scalar multiplication of the weight vecotr
prod :: Float -> Weight  -> Weight
prod yn (x0,x1,x2)  = (x0*yn,x1*yn,x2*yn)

-- | Converting a point to a weight by adding the first coordinate (1)
point2weight :: Point -> Weight
point2weight (x1,x2) =(1,x1,x2)

-----------------------------------
-- creating label and calculating error
------------------------------------

-- | looks up the label of the point by applying the target function
y :: (Float -> Float) -> Point -> Label
y yLine (x1,x2) = if yLine x1 < x2 then (-1) else 1

-- | The CrossEntropy error of one point
crossEntropy1'
  ::  Weight -> LabelledPoint -> Float
crossEntropy1' w (LabelledPoint x y1) = log (1+ exp(-1*y1 *dotProd xw w))
    where xw = point2weight x


 -- | The CrossEntropy error of a list of points 
crossEntropy'
  ::  Weight -> [LabelledPoint] -> Float
crossEntropy' w points = (*) 0.01 $ sum $ map (crossEntropy1' w) points

-- | The gradient of the CrossEntropy with respect to the weight for one point
dCrossEntropy1' :: Weight -> LabelledPoint -> Weight
dCrossEntropy1' w (LabelledPoint x y1) = prod (-1/(1+ exp(y1* dotProd xw w)))(prod y1 xw ) 
    where xw = point2weight x
-----------------------------------
-- Training
------------------------------------
-- | The update function for the weight
updateWeight'
  :: Float -> Weight -> LabelledPoint -> Weight
updateWeight' eta w labelledpoint =  w - prod  eta (dCrossEntropy1' w labelledpoint) 

-- | Performs 1 training epoch (one run through all training points)
epoch'
  :: Float -> Weight -> [LabelledPoint] -> Weight
epoch' eta = foldl' (updateWeight' eta) 


-- | performs training epochs until the stop criterion for convergence is reached
finalWeights'
  :: Float
     -> Float
     -> Weight
     -> [[ LabelledPoint]]
     -> Int
     -> (Weight, Int)
finalWeights' stopcrit eta weight0 pointpermutations epochs
    |vecval (weight1 -weight0)<stopcrit = (weight1,epochs)
    |otherwise = finalWeights' stopcrit eta weight1 (tail  pointpermutations) (epochs+1)
        where 
            weight1 = epoch' eta weight0 $head pointpermutations
            vecval (w0,w1,w2) = sqrt $ w0**2 + w1**2 + w2**2
-----------------------------------
-- Creating random data points
------------------------------------

-- |Creates a random Point between (-1,-1) and (1,1)
createRandomPoint :: IO (Float, Float)
createRandomPoint = do
    rand1 :: Float <- randomRIO (-1,1)
    rand2 :: Float <- randomRIO (-1,1)
    return  (rand1,rand2)

-- |Creates a list of n random Points between (-1,-1) and (1,1)
createRandomPoints :: Int -> IO [(Float, Float)]
createRandomPoints n  = do
    randList1 :: [Float] <- for [1 .. n] $ \_ -> randomRIO (-1,1)
    randList2 :: [Float] <- for [1 .. n] $ \_ -> randomRIO (-1,1)
    return $ zip randList1 randList2

 -- | creates an infinite list of permutations of the list provided as an argument  
pointPermutations :: [a] -> [[a]]
pointPermutations = cycle . permutations 

---------------------------------------------------------------
-- The final loop that creates the target function and training and testing data,
-- performs training and testing
-------------------------------------------------------------------

-- | performs the whole training and testing loop, printing the weights, epochs and the test error
trainAndTest :: IO ()
trainAndTest = do
    point1 <- createRandomPoint
    point2 <- createRandomPoint
    let target = line point1 point2
    trainpoints <- createRandomPoints 100
    let labelpoints x = LabelledPoint x (y target x)
    let labelledpointsTrain = map labelpoints  trainpoints
    let pointpermutations = pointPermutations labelledpointsTrain
    let (weights,epochs) = finalWeights' 0.01 0.01 (0,0,0) pointpermutations 0
    putStr "Weights: "
    print weights
    putStr "Epochs: "
    print epochs
    testpoints <- createRandomPoints 100
    let labelledpointsTest = map labelpoints  testpoints
    let testError = crossEntropy' weights labelledpointsTest
    putStr "Eout: "
    print testError

