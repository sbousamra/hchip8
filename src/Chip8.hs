module Chip8
    ( Chip8 (..)
    , Opcode (..)
    , create
    , cls
    , ret
    , jp
    , executeOpcode
    , emulate
    ) where

import Data.Word
import Data.Bits

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
  | JP

create :: Chip8
create = Chip8 {
  memory = [],
  programCounter = 0x200,
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

executeOpcode :: Chip8 -> Opcode -> Chip8
executeOpcode chip8 opcode = case opcode of
  CLS -> cls chip8
  RET -> ret chip8
  JP -> jp chip8

emulate :: Chip8 -> IO ()
emulate chip8 = do
  print (programCounter chip8)
  emulate (executeOpcode chip8 CLS)