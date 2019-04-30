import Test.Tasty
import Test.Tasty.HUnit
import Chip8

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [opcodeTests]

opcodeTests :: TestTree
opcodeTests = testGroup "Opcode Tests" [clsTest, retTest]

clsTest :: TestTree
clsTest = testCase "CLS" $ do
  let chip8 = spawn {
    screen = [[1, 2, 3]]
  }
  let expected = chip8 {
    screen = [[]]
  }
  let actual = cls chip8
  expected @=? actual

retTest :: TestTree
retTest = testCase "RET" $ do
  let chip8 = spawn {
    stack = [1, 2],
    sp = 1,
    pc = 0
  }
  let expected = chip8 {
    sp = 0,
    pc = 2
  }
  let actual = ret chip8
  expected @=? actual
