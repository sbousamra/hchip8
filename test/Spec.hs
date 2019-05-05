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
  let subject = create {
    programCounter = 0,
    screen = [[1, 2, 3]]
  }
  let expected = subject {
    programCounter = 2,
    screen = [[]]
  }
  let actual = cls subject
  expected @=? actual

retTest :: TestTree
retTest = testCase "RET" $ do
  let subject = create {
    stack = [1, 2],
    stackPointer = 1,
    programCounter = 0
  }
  let expected = subject {
    stackPointer = 0,
    programCounter = 2
  }
  let actual = ret subject
  expected @=? actual

jpTest :: TestTree
jpTest = testCase "JP" $ do
  let subject = create
  let expected = subject {
    programCounter = 0x234
  }
  let actual = jp subject 0x1234
  expected @=? actual