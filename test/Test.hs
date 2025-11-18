module Main where

import Test.Tasty
import qualified JsonParserTest
import qualified TapeValidationTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ftTuring Tests"
    [ JsonParserTest.tests
    , TapeValidationTest.tests
    ]
