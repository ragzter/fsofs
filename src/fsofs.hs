
import RNN
import Sound
import Convert

import System.IO
import qualified Data.ByteString.Lazy as BL

-- Compute sound and write to file.
main :: IO ()
main = do
  handle <- openFile "test.raw" WriteMode
  BL.hPut handle $ iltob $ to32BitL $ lowpass (saw 440 1147483647 1000) 10 -- Big endian output
  hClose handle
