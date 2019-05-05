module Chip8
  ( Chip8 (..)
  , Opcode (..)
  , create
  , cls
  , ret
  , jp
  , executeOpcode
  , emulate
  , start
  ) where

import Data.Word
import Data.Bits
import Data.List (genericIndex)
import Data.ByteString as BS
import System.Directory

type RomName = String
data Chip8 = Chip8 {
  memory :: [Word8],
  programCounter :: Word16,
  stackPointer :: Int,
  stack :: [Word16],
  vRegister :: [Word16],
  iRegister :: Word16,
  soundTimer :: Word16,
  delayTimer :: Word16,
  keyboardInput :: [Word16],
  screen :: [[Word16]]
} deriving (Eq, Show)

data Opcode
  = CLS
  | RET
  | JP {
    rawOpcode :: Word16
  }

create :: Chip8
create = Chip8 {
  memory = [],
  programCounter = 0,
  stackPointer = 0,
  stack = [0],
  vRegister = [],
  iRegister = 0x0,
  soundTimer = 0,
  delayTimer = 0,
  keyboardInput = [],
  screen = [[]]
}

cls :: Chip8 -> Chip8
cls chip8 = chip8 {
  programCounter = programCounter chip8 + 2,
  screen = [[]]
}

ret :: Chip8 -> Chip8
ret chip8 = chip8 {
  programCounter = stack chip8 !! stackPointer chip8,
  stackPointer = stackPointer chip8 - 1
}

jp :: Chip8 -> Word16 -> Chip8
jp chip8 rawOpcode = chip8 {
  programCounter = rawOpcode .&. 0x0fff
}

decodeOpcode :: Word16 -> Opcode
decodeOpcode opcode = do
  -- Use the first nibble of byte as identifier.
  let decodedOpcode = opcode .&. 0xf000
  case decodedOpcode of
    -- Different opcodes can start with the same nibble, so need a
    -- subset identifier in some cases.
    0x0000 -> do
      let zeroDecodedOpcode = opcode .&. 0x00ff
      case zeroDecodedOpcode of
        0x00e0 -> CLS
        0x00ee -> RET
    0x1000 -> JP {
      rawOpcode = opcode
    }

executeOpcode :: Chip8 -> Opcode -> Chip8
executeOpcode chip8 opcode = case opcode of
  CLS -> cls chip8
  RET -> ret chip8
  JP rawOpcode -> jp chip8 rawOpcode

emulate :: Chip8 -> IO ()
emulate chip8 = do
  let opcode = fromIntegral (memory chip8 `genericIndex` programCounter chip8 - 1)
        .|. fromIntegral (memory chip8 `genericIndex` programCounter chip8)
  let decodedOpcode = decodeOpcode opcode
  emulate (executeOpcode chip8 decodedOpcode)

readRom :: FilePath -> IO [Word8]
readRom filePath = do
  byteString <- BS.readFile filePath
  pure (BS.unpack byteString)

start :: RomName -> IO ()
start romName = do
  dir <- getCurrentDirectory
  rom <- readRom (dir ++ "/roms/" ++ romName ++ ".ch8")
  let chip8 = create {
    memory = rom
  }
  print chip8
  emulate chip8

