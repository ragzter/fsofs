
module Sound
( highpass
, lowpass
, saw
, sine
, mix
) where

import Convert

sampleRate = 44100
channel = []
maxVelocity = 2147483647

-- Compute lowpass filter on a list of integers for a given range.
lowpass :: [Integer] -> Integer -> [Integer]
lowpass [] _ = []
lowpass xs s = (arithmeticMean s xs) : (lowpass (tail xs) s)

-- Compute highpass filter on a list of integers for a given range.
highpass :: [Integer] -> Integer -> [Integer]
highpass [] _ = []
highpass xs s = zipWith (-) xs (lowpass xs s)

-- Take a range and a list of integers as arguments and compute
-- arithmetic mean from beginning of list.
arithmeticMean :: Integer -> [Integer] -> Integer
arithmeticMean r = (flip div r) . sum . take ri
  where ri = (fromIntegral r) :: Int

-- Generate sawtooth wave at given frequency, velocity and period (in
-- milliseconds).
saw :: Integer -> Integer -> Integer -> [Integer]
saw f v p = map ((*) rv .
                 flip (-) (rdf `div` 2) .
                 flip mod rdf) [0..ss]
  where ss = msToNSamples p
        rdf = sampleRate `div` f
        rv = v `div` (rdf `div` 2)

mix :: [Integer] -> [Integer] -> [Integer]
mix = zipWith (+)

-- Generate sine wave at given frequency, velocity and period (in
-- milliseconds).
sine :: Integer -> Integer -> Integer -> [Integer]
sine f v p = map (round .
                  (*) (realToFrac v) .
                  sin .
                  (flip (/)) (fromIntegral sampleRate) .
                  (*) (fromIntegral f) .
                  (*) pi .
                  (*) 2 .
                  fromIntegral) [0..ss]
  where ss = msToNSamples p

-- Convert time in milliseconds to number of samples.
msToNSamples :: Integer -> Integer
msToNSamples ms = ms * sampleRate `div` 1000
