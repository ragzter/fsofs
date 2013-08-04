
module RNN
( setValueInNet
, createNetwork
, createWeights
, step
) where

import Data.List
import System.Random

data Node = N Int Int

maxAmplitude = 2147483647 :: Int

-- Make one random node-to-node computation.
step :: IO [Node] -> IO [Double] -> IO [Node]
step ns ws = do
  pns <- ns
  pws <- ws
  is <- uniqueIndices $ length pns
  let cn = pns !! (fst is)
      hn = pns !! (snd is)
      t = getThreshold hn
      cv = getValue cn
      w = getWeight (fst is) (snd is) pws
      str = round $ (fromIntegral cv) * w
  if str > t
    then setValueInNet str (snd is) ns
    else ns

-- Compute two unique numbers with a given maximum.
uniqueIndices :: Int -> IO (Int, Int)
uniqueIndices m = do
  i <- randomRIO(0, m - 1)
  r <- randomRIO(0, m - 2)
  let j = (except i [0..m]) !! r
  return (i, j)

-- Print value in network at given index.
printValue :: Int -> IO [Node] -> IO ()
printValue i ns = do
  n <- fmap (!! i) ns
  print (getValue n)

-- Set Node value for a network at a given index.
-- Todo: Give this function a better name
setValueInNet :: Int -> Int -> IO [Node] -> IO [Node]
setValueInNet v i ns = do
  n <- fmap (!! i) ns
  pns <- ns
  return $ update (setValue v n) i pns

-- Change element in List at given index.
update :: a -> Int -> [a] -> [a]
update v i xs = (take i xs) ++ [v] ++ (drop (i + 1) xs)

-- Get Node value.
getValue :: Node -> Int
getValue (N _ v) = v

-- Get Node threshold.
getThreshold :: Node -> Int
getThreshold (N t _) = t

-- Change Node value.
setValue :: Int -> Node -> Node
setValue nv (N t v) = (N t nv)

-- Create network of given size.
createNetwork :: Int -> IO [Node]
createNetwork 0 = return []
createNetwork n = do
  rt <- randomRIO (0, maxAmplitude)
  ns <- createNetwork (n - 1)
  return $ (N rt 0) : ns

-- Create weights for RNN.
-- Note: In ghci, these values change each time they are requested,
-- but this should not happen in the final program because the
-- function will only be called once.
createWeights :: Int -> IO [Double]
createWeights n = sequence $
                  map randomRIO $
                  replicate (n * (n - 1)) (0.5, 2.0)

-- Get weight between two nodes for RNN.
getWeight :: Int -> Int -> [Double] -> Double
getWeight a b ws = ws !! ((a * b) - 1)

-- Print weights since Show apparently doesn't work with Lists with
-- IO elements.
-- Note: Deprecated (due to IO sequencing)
printWeights :: [IO Double] -> IO ()
printWeights [w] = do
  p <- w
  print p
printWeights (w:ws) = do
  p <- w
  print p
  printWeights ws

-- Takes a list and returns the same list except element at given
-- index.
except :: Int -> [a] -> [a]
except i xs = take i xs ++ drop (i + 1) xs
