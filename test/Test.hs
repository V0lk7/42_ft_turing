module Main where

import Test.Tasty
import qualified JsonParserTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ftTuring Tests"
    [ JsonParserTest.tests ]
