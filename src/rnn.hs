
module RNN
( createNetwork
, step
, values
, mesh
) where

import Data.List
import System.Random
import Control.Monad

data Network = Network { values :: [Integer]
                       , thresholds :: [Integer]
                       , weights :: [Double]
                       } deriving Show

maxAmplitude = 2147483647 :: Integer

-- Evolve network and append evolved values to a list which is
-- returned.
mesh :: Network -> Integer -> IO [Integer]
mesh n t = mesh' n t []
  where
    mesh' :: Network -> Integer -> [Integer] -> IO [Integer]
    mesh' n 1 vs = do
      nn <- step n
      return $ values nn
    mesh' n t vs = do
      nn <- step n
      nivs <- mesh' n (t - 1) vs
      let nvs = values nn
      return $ nvs ++ nivs

-- Make one random neural computation.
step :: Network -> IO Network
step n = do
  is <- (uniqueIndices (length (values n)))
  let cv = (values n) !! (fst is)
      w = getWeight (fst is) (snd is) n
      t = (thresholds n) !! (snd is)
      str = round $ (fromIntegral cv) * w
  if str > t
    then return $ Network (update str (snd is) (values n)) (thresholds n) (weights n)
         else return n

-- Compute two unique numbers with a given maximum.
uniqueIndices :: Int -> IO (Int, Int)
uniqueIndices m = do
  i <- randomRIO(0, m - 1)
  r <- randomRIO(0, m - 2)
  let j = (except i [0..(m - 1)]) !! r
  return (i, j)

-- Get weight between two nodes for RNN.
getWeight :: Int -> Int -> Network -> Double
getWeight a b n = if i < 0
                  then (weights n) !! 0
                  else (weights n) !! i
  where i = (a * (b - 1)) - 1

-- Change element in List at given index.
update :: a -> Int -> [a] -> [a]
update v i xs = (take i xs) ++ [v] ++ (drop (i + 1) xs)

-- Create Network of given size.
createNetwork :: Integer -> IO Network
createNetwork n = do
  vs <- randomIntegers n (-maxAmplitude) maxAmplitude
  ts <- randomIntegers n (-maxAmplitude) maxAmplitude
  ws <- randomIntegers (n * (n - 1)) 0.5 2.0
  return $ Network vs ts ws

-- Generate a List of random numbers.  Takes length, min and max as
-- parameters.
-- Note: `replicate' could be used instead of recursion.
randomIntegers :: (Random a) => Integer -> a -> a -> IO [a]
randomIntegers 0 _ _ = return []
randomIntegers r f t = do
  rt <- randomRIO (f, t)
  xs <- randomIntegers (r - 1) f t
  return $ rt : xs

-- Takes a list and returns the same list except element at given
-- index.
except :: Int -> [a] -> [a]
except i xs = take i xs ++ drop (i + 1) xs
