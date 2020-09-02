{-#LANGUAGE ScopedTypeVariables #-}
module CoinFlips (
    randomFlips,
    firstCoin,
    randomCoin,
    numHeadsCoin,
    minHeadsCoin,
    vmin

)
where 
import System.Random
import Data.List.Split (chunksOf)
import Control.Monad (replicateM)
----------------------------------------------------
-- Solutions to homework 2 of "Learning from data"
----------------------------------------------------

-----------------------------------------------------
-- Flipping coins
------------------------------------------------------

-- | The Coin Data Type
data Coin = Head | Number deriving (Show,Read,Eq)

-- | Flipping 1000 fair coins 10 times each - returns an IO
-- | action of a list of 1000 lists, each with 10 results 'Head' or 'Number'
randomFlips :: IO [[Coin]]
randomFlips = do
    gen <- getStdGen
    let randnums :: [Int]= randomRs (0,1) gen 
    let flips = map randNumToCoin randnums
    let coins = chunksOf 10 flips
    let coins1000 = take 1000 coins
    return coins1000

-- | The first coin in a list of coins
firstCoin :: IO [[Coin]]-> IO [Coin]
firstCoin  = fmap head 

-- | A random coin
randomCoin :: IO [Coin]
randomCoin = do
    coins1000 <- randomFlips
    randnum <- randomRIO (0,999)
    let randcoin = coins1000 !! randnum
    return randcoin

-- |Converts 0 or 1 to the Coin datatype
randNumToCoin :: Int -> Coin
randNumToCoin 1 = Head
randNumToCoin 0 = Number 

-- | Counts the Heads in a list of CoinFlip results
countHeads :: [Coin] -> Int
countHeads  = length.filter (==Head)

-- | returns the minimum number of Heads in a list of list of CoinFlip results
minHeads :: [[Coin]] -> Int
minHeads coins = minimum $ map (length.filter (==Head)) coins

-- |calculates the fraction of Heads from the number of Heads, assuming 10 tosses
fracHeads :: Int -> Float
fracHeads count = fromIntegral count/10

-- | calculates the fraction of Heads in the coin with minimum number of Heads wrapped in an IO action
minHeadsCoin :: IO [[Coin]] -> IO Float
minHeadsCoin = fmap (fracHeads . minHeads)

-- | calculates the fraction of Heads in a list of CoinFlip results wrapped in an IO action
numHeadsCoin :: IO [Coin] -> IO Float
numHeadsCoin = fmap (fracHeads . countHeads)

-- | average in a list of numbers
average :: [Float] -> Float
average xs = sum xs / fromIntegral (length xs)

-- | minimum fraction of Heads of 1000 coins tossed 10 times each, repeated n times. This is quite inefficient - don't run with n > 1000
vmin :: Int -> IO Float
vmin n = fmap average $ replicateM n $ minHeadsCoin randomFlips