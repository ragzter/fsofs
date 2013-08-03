
-- Todo: Flip parameters on either `lowpass' or `arithmeticMean'?

module Sound
( highpass
, lowpass
, saw
, sine
) where

sampleRate = 44100
channel = []
maxVelocity = 2147483647

-- Compute highpass filter on a list of integers for a given range.
-- Defunct
highpass :: [Int] -> Int -> [Int]
highpass [x] _ = [x]
highpass xs r = arithmeticMean r xs : highpass (tail xs) r

-- Compute lowpass filter on a list of integers for a given range.
-- Defunct
lowpass :: [Int] -> Int -> [Int]
lowpass xs r = reverse $ highpass (reverse xs) r

-- Take a range and a list of integers as arguments and compute
-- arithmetic mean from beginning of list.
arithmeticMean :: Int -> [Int] -> Int
arithmeticMean r = flip div r . sum . take r

-- Generate sawtooth wave at given frequency, velocity and period (in
-- milliseconds).
saw :: Int -> Int -> Int -> [Int]
saw f v p = map ((*) rv .
                 flip (-) (rdf `div` 2) .
                 flip mod rdf) [0..ss]
  where ss = msToNSamples p
        rdf = sampleRate `div` f
        rv = v `div` (rdf `div` 2)

-- Generate sine wave at given frequency, velocity and period (in
-- milliseconds).
sine :: Int -> Int -> Int -> [Int]
sine f v p = map (round .
                  (*) (realToFrac v) .
                  sin .
                  (flip (/)) (fromIntegral sampleRate) .
                  (*) (fromIntegral f) .
                  (*) pi .
                  (*) 2 .
                  fromIntegral) [0..ss] :: [Int]
  where ss = msToNSamples p

-- Convert time in milliseconds to number of samples.
msToNSamples :: Int -> Int
msToNSamples ms = ms * sampleRate `div` 1000
