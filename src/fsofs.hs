
import RNN
import Sound
import Convert

import System.IO
import qualified Data.ByteString.Lazy as BL

-- Compute sound and write to file.
main :: IO ()
main = do
  handle <- openFile "test.raw" WriteMode
  BL.hPut handle
    $ iltob
    $ to32BitL
    -- $ saw 140 114748364 1000
    $ lowpass (saw 140 114748364 1000) 1 -- Big endian output
  hClose handle
