module Lib
    ( Chip8 (..)
    , Opcode (..)
    , spawn
    , cls
    , ret
    , executeOpcode
    , emulate
    ) where

import Data.Word

data Chip8 = Chip8 {
  memory :: [Word8],
  pc :: Word16,
  sp :: Int,
  stack :: [Word16],
  v :: [Word16],
  i :: Word16,
  st :: Word16,
  dt :: Word16,
  ki :: [Word16],
  s :: [[Word16]]
} deriving (Eq, Show)

data Opcode
  = CLS
  | RET

spawn :: Chip8
spawn = Chip8 {
  memory = [],
  pc = 0x200,
  sp = 0,
  stack = [0],
  v = [],
  i = 0x0,
  st = 0,
  dt = 0,
  ki = [],
  s = [[]]
}

cls :: Chip8 -> Chip8
cls chip8 = chip8 {
  s = [[]]
}

ret :: Chip8 -> Chip8
ret chip8 = chip8 {
  pc = ((stack chip8)!!(sp chip8)),
  sp = sp chip8 - 1
}

executeOpcode :: Chip8 -> Opcode -> Chip8
executeOpcode chip8 opcode = case opcode of
  CLS -> cls chip8
  RET -> ret chip8

emulate :: Chip8 -> IO ()
emulate chip8 = do
  print (pc chip8)
  emulate (executeOpcode chip8 CLS)