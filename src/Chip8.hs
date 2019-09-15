module Chip8
  ( Chip8 (..)
  , create
  , cls
  , ret
  , jp
  , decodeOpcode
  , executeOpcode
  , step
  , start
  ) where

import Data.Bits
import Data.ByteString as BS
import Data.List (genericIndex)
import Data.Word
import System.Directory

{-|
  All data types, function definitions and implementations are based on
  the documentation provided at http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#00E0
  and http://mattmik.com/files/chip8/mastering/chip8.html.
-}

type RomPath = String
type RomName = String

data Chip8 = Chip8 {
  memory         :: [Word8],
  programCounter :: Word16,
  stackPointer   :: Int,
  stack          :: [Word16],
  vRegister      :: [Word16],
  iRegister      :: Word16,
  soundTimer     :: Word16,
  delayTimer     :: Word16,
  keyboardInput  :: [Word16],
  screen         :: [[Word16]]
} deriving (Eq, Show)

newtype NNN = NNN Word16
newtype N = N Word8
newtype X = X Word8
newtype Y = Y Word8
newtype KK = KK Word8

data Opcode
  = CLS
  | RET
  | JP NNN

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

{-|
  00E0 - CLS
  Clear the display
-}

cls :: Chip8 -> Chip8
cls chip8 = chip8 {
  programCounter = programCounter chip8 + 2,
  screen = [[]]
}

{-|
  00EE - RET
  Return from a subroutine.
-}

ret :: Chip8 -> Chip8
ret chip8 = chip8 {
  programCounter = stack chip8 !! stackPointer chip8,
  stackPointer = stackPointer chip8 - 1
}

{-|
  1nnn - JP addr
  Jump to location nnn.
-}

jp :: Chip8 -> NNN -> Chip8
jp chip8 (NNN v)= chip8 {
  programCounter = v
}

decodeOpcode :: Word16 -> Opcode
decodeOpcode opcode = do
  -- Use the first nibble of byte as identifier.
  let decodedOpcode = opcode .&. 0xf000
  case decodedOpcode of
    -- Different opcodes can start with the same nibble. We need a
    -- subset identifier in some cases.
    0x0000 -> do
      let zeroDecodedOpcode = opcode .&. 0x00ff
      case zeroDecodedOpcode of
        0x00e0 -> CLS
        0x00ee -> RET
    0x1000 -> do
      let nnn = NNN (opcode .&. 0x0fff)
      JP nnn

executeOpcode :: Chip8 -> Opcode -> Chip8
executeOpcode chip8 opcode = case opcode of
  CLS    -> cls chip8
  RET    -> ret chip8
  JP nnn -> jp chip8 nnn

step :: Chip8 -> IO ()
step chip8 = do
  let opcode = fromIntegral (memory chip8 `genericIndex` programCounter chip8 - 1)
        .|. fromIntegral (memory chip8 `genericIndex` programCounter chip8)
  let decodedOpcode = decodeOpcode opcode
  step (executeOpcode chip8 decodedOpcode)

readRom :: RomPath -> IO [Word8]
readRom romPath = do
  byteString <- BS.readFile romPath
  pure (BS.unpack byteString)

start :: RomName -> IO ()
start romName = do
  dir <- getCurrentDirectory
  rom <- readRom (dir ++ "/roms/" ++ romName)
  let chip8 = create {
    memory = rom
  }
  step chip8

