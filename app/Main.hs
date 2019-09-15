module Main where

import Chip8
import Data.Word
import System.Directory
import Data.ByteString as BS

trace :: Chip8 -> IO ()
trace = print

readRom :: String -> IO [Word8]
readRom romPath = do
  byteString <- BS.readFile romPath
  pure (BS.unpack byteString)

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rom <- readRom (dir ++ "/roms/ch8test.ch8")
  trace $ start rom
