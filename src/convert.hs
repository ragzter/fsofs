
module Convert
( iltob
, to32Bit
, to32BitL
) where

import Data.Int
import Data.Binary
import qualified Data.ByteString.Lazy as BL

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
to32BitL = map to32Bit
