import Test.Tasty
import Test.Tasty.HUnit
import Chip8

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [opcodeTests]

opcodeTests :: TestTree
opcodeTests = testGroup "Opcode Tests" [clsTest, retTest, jpTest]

clsTest :: TestTree
clsTest = testCase "CLS" $ do
  let chip8 = create {
    screen = [[1, 2, 3]]
  }
  let expected = chip8 {
    screen = [[]]
  }
  let actual = cls chip8
  expected @=? actual

retTest :: TestTree
retTest = testCase "RET" $ do
  let chip8 = create {
    stack = [1, 2],
    stackPointer = 1,
    programCounter = 0
  }
  let expected = chip8 {
    stackPointer = 0,
    programCounter = 2
  }
  let actual = ret chip8
  expected @=? actual

jpTest :: TestTree
jpTest = testCase "JP" $ do
  let chip8 = create
  let expected = chip8 {
    programCounter = 0x234
  }
  let actual = jp chip8 0x1234
  expected @=? actual