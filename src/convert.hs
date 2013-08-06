
module Convert
( iltob
, to32Bit
, to32BitL
) where

import Data.Int
import Data.Binary
import qualified Data.ByteString.Lazy as BL

maxAmplitude = 2147483647 :: Integer

-- Convert List of Int32 to ByteString.
iltob :: [Int32] -> BL.ByteString
iltob [x] = encode x
iltob (x:xs) = BL.append (encode x) (iltob xs)

-- Convert Int to Int32
-- Todo: Check if clipping?
to32Bit :: Integer -> Int32
to32Bit x = (fromIntegral x) :: Int32

-- Convert List of Int to List of Int32
to32BitL :: [Integer] -> [Int32]
to32BitL xs = if (maximum xs) > maxAmplitude then
                map to32Bit (modulate xs (-maxAmplitude) maxAmplitude)
              else
                map to32Bit xs

-- Takes integers and modulates it so that the content is within given
-- range (lower and upper boundaries)
modulate :: [Integer] -> Integer -> Integer -> [Integer]
modulate vs l u = map (modulate' range min l u) vs
  where min = minimum vs
        max = maximum vs
        range = max - min

modulate' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
modulate' r m l u x = ((x + (-m)) `div` (r `div` u)) + l
