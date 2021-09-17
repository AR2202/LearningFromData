{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PLA
  ( Weight(..)
  , Point'(..)
  , viewX
  , trainPLA
  , trainPLAWithInitial
  , isMisclassified
  ) where

import           Control.Lens
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Data.List
import           Data.Traversable           (for)
import           Numeric.LinearAlgebra.Data
import           System.Random

--------------------------------
--  Solutions to homework 1, problems 7-10 of
--  the 'Learning from Data' course
-- http://work.caltech.edu/homeworks.html
-------------------------------------
-------------------------------
-- Types and Typeclass Instances
--------------------------------
type Weight = (R, R, R)

type Point = (R, R)

-- | To be used at a later stage - currently just a test
data Point' =
  Point'
    { _x :: R
    , _y :: R
    }

makeLenses ''Point'

instance Num Weight where
  (+) (x0, x1, x2) (w0, w1, w2) = (w0 + x0, w1 + x1, w2 + x2)
  (-) (x0, x1, x2) (w0, w1, w2) = (x0 - w0, x1 - w1, x2 - w2)
  (*) (x0, x1, x2) (w0, w1, w2) =
    (w2 * x1 - w1 * x2, w2 * x0 - w0 * x2, w1 * x0 - w0 * x1)
  negate (w0, w1, w2) = (negate w0, negate w1, negate w2)
  abs (w0, w1, w2) = (abs w0, abs w1, abs w2)
  fromInteger x = (fromInteger x, 0.0, 0.0)
  signum (w0, w1, w2) = signum (w0, w1, w2)

--------------------------------------------
-- Creating the line for the target function
--------------------------------------------
m :: Point -> Point -> R
m (x1, x2) (z1, z2) = (x2 - z2) / (x1 - z1)

b :: Point -> Point -> R
b (x1, x2) (z1, z2) = x2 - x1 * (x2 - z2) / (x1 - z1)

-- | Takes 2 Points and returns the function of a line connecting them
line :: Point -> Point -> (R -> R)
line point1 point2 = \x -> m point1 point2 * x + b point1 point2

-----------------------------------
-- functions for the Weight type
-----------------------------------
-- | The dot product
dotProd :: Weight -> Weight -> R
dotProd (x0, x1, x2) (w0, w1, w2) = w0 * x0 + w1 * x1 + w2 * x2

-- | Scalar multiplication of the weight vecotr
prod :: R -> Weight -> Weight
prod lambda (x0, x1, x2) = (x0 * lambda, x1 * lambda, x2 * lambda)

-- | Converting a point to a weight by adding the first coordinate (1)
point2weight :: Point -> Weight
point2weight (x1, x2) = (1, x1, x2)

-- | looks up the label of the point by applying the target function
yn :: (R -> R) -> Point -> R
yn yLine (x1, x2) =
  if yLine x1 < x2
    then (-1)
    else 1

-- | Checking if a point is misclassified
isMisclassified yLine weight point = classified /= label
  where
    classified = signum $ dotProd (point2weight point) weight
    label = yn yLine point

updateWeight' yLine w xn = w + prod (yn yLine xn) (point2weight xn)

-- | Performs 1 training epoch (one run through all training points)
-- | performs the perceptron learning algorithm
pla yLine w niter gen xs
  | misclassifiedXs == [] = (w, niter)
  | otherwise = pla yLine wUpdated (niter + 1) nextRandom xs
  where
    wUpdated = updateWeight' yLine w $ misclassifiedXs !! number
    misclassifiedXs = filter (isMisclassified yLine w) xs
    randomRange = (0, length misclassifiedXs - 1)
    (number, nextRandom) = randomR randomRange gen

-- | stores the target function and performs training, returning the weight and the number of epochs performed before convergence
weightReader' :: [Point] -> StdGen -> Reader (R -> R) (Weight, Int)
weightReader' points gen = do
  targetFunc <- ask
  let weightsEpochs = pla targetFunc (0, 0, 0) 0 gen points
  return weightsEpochs

-- | stores the target function and performs training, returning the weight and the number of epochs performed before convergence
weightReader :: [Point] -> StdGen -> Weight -> Reader (R -> R) (Weight, Int)
weightReader points gen initialWeights = do
  targetFunc <- ask
  let weightsEpochs = pla targetFunc initialWeights 0 gen points
  return weightsEpochs

-- |Creates a random Point between (-1,-1) and (1,1)
createRandomPoint = do
  rand1 :: R <- randomRIO (-1, 1)
  rand2 :: R <- randomRIO (-1, 1)
  return (rand1, rand2)

-- |Creates a list of n random Points between (-1,-1) and (1,1)
createRandomPoints n = do
  randList1 :: [R] <- for [1 .. n] $ \_ -> randomRIO (-1, 1)
  randList2 :: [R] <- for [1 .. n] $ \_ -> randomRIO (-1, 1)
  return $ zip randList1 randList2-- | creates an infinite list of permutations of the list provided as an argument

pointPermutations = cycle . permutations

-- | Just a test for the point lens
viewX point = view x point

---------------------------------------------------------------
-- The final loop that creates the target function and training and testing data,
-- performs training and testing
-------------------------------------------------------------------
-- | performs the PLA
trainPLA :: Int -> IO ()
trainPLA nPoints = do
  point1 <- createRandomPoint
  point2 <- createRandomPoint
  stdgen <- getStdGen
  let target = line point1 point2
  trainpoints <- createRandomPoints nPoints
  let (weights, epochs) = runReader (weightReader' trainpoints stdgen) target
  putStr "Weights: "
  print weights
  putStr "Epochs: "
  print epochs
  testpoints <- createRandomPoints 1000
  let misclassifiedTestPoints =
        length $ filter (isMisclassified target weights) testpoints
  let prob = fromIntegral misclassifiedTestPoints / 1000
  putStr "Probability f /= g: "
  print prob

-- | performs the PLA with a set of initial weights, training points and a target function
trainPLAWithInitial :: (R, R, R) -> [Point] -> (R -> R) -> IO (Weight, Int)
trainPLAWithInitial initialWeights trainPoints target = do
  stdgen <- getStdGen
  let (weights, epochs) =
        runReader (weightReader trainPoints stdgen initialWeights) target
  return (weights, epochs)
