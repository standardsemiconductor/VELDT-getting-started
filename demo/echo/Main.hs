import System.Hardware.Serialport
import System.IO
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = withSerial "/dev/ttyUSB0" settings $ \port -> do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  forever $ echo port
  where
    echo port = do
      send port . B.singleton =<< getChar
      putChar . B.head =<< recv port 1
    settings = defaultSerialSettings{ commSpeed = CS19200 }
