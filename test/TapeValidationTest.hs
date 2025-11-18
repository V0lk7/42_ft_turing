{-# LANGUAGE OverloadedStrings #-}
module TapeValidationTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Json_Parser as JP
import qualified Data.Map.Strict as M
import TapeValidator (validateTape)
    

-- ====================
-- Machine Definitions
-- ====================

unaryAdditionMachine :: JP.Machine
unaryAdditionMachine = JP.Machine
    { JP.mName = "unary_addition"
    , JP.mAlphabet = ["1", "+", "."]
    , JP.mBlank = "."
    , JP.mStates = ["s1", "s2"]
    , JP.mInitial = "s1"
    , JP.mCurrent = "s1"
    , JP.mFinals = ["s2"]
    , JP.mTransitions = M.empty
    }

zeroNoneNMachine :: JP.Machine
zeroNoneNMachine = JP.Machine
    { JP.mName = "0n1n"
    , JP.mAlphabet = ["0", "1", "."]
    , JP.mBlank = "."
    , JP.mStates = ["s1", "s2"]
    , JP.mInitial = "s1"
    , JP.mCurrent = "s1"
    , JP.mFinals = ["s2"]
    , JP.mTransitions = M.empty
    }

zeroTwoNMachine :: JP.Machine
zeroTwoNMachine = JP.Machine
    { JP.mName = "02n"
    , JP.mAlphabet = ["0", "."]
    , JP.mBlank = "."
    , JP.mStates = ["s1", "s2"]
    , JP.mInitial = "s1"
    , JP.mCurrent = "s1"
    , JP.mFinals = ["s2"]
    , JP.mTransitions = M.empty
    }

palindromeMachine :: JP.Machine
palindromeMachine = JP.Machine
    { JP.mName = "palindrome"
    , JP.mAlphabet = ["0", "1", "."]
    , JP.mBlank = "."
    , JP.mStates = ["s1", "s2"]
    , JP.mInitial = "s1"
    , JP.mCurrent = "s1"
    , JP.mFinals = ["s2"]
    , JP.mTransitions = M.empty
    }

-- ====================
-- Test Data
-- ====================

data TapeTestCase = TapeTestCase
    { tcName :: String
    , tcMachine :: JP.Machine
    , tcTape :: String
    , tcShouldSucceed :: Bool
    , tcExpectedInMessage :: [String]
    }

allTestCases :: [TapeTestCase]
allTestCases =
    -- Unary Addition Tests
    [ TapeTestCase "Unary: valid input 11+111=" unaryAdditionMachine "11+111" True []
    , TapeTestCase "Unary: valid input 1+1=" unaryAdditionMachine "1+1" True []
    , TapeTestCase "Unary: reject blank" unaryAdditionMachine "11+.11" False ["blank character"]
    , TapeTestCase "Unary: reject invalid char '0'" unaryAdditionMachine "11+011" False ["Invalid characters", "0"]
    , TapeTestCase "Unary: reject invalid char 'a'" unaryAdditionMachine "1a+1" False ["Invalid characters", "a"]
    , TapeTestCase "Unary: empty tape" unaryAdditionMachine "" True []
    , TapeTestCase "Unary: only blank" unaryAdditionMachine "...+..." False ["blank character"]
    
    -- 0^n1^n Tests
    , TapeTestCase "0n1n: valid input 0011" zeroNoneNMachine "0011" True []
    , TapeTestCase "0n1n: valid input 000111" zeroNoneNMachine "000111" True []
    , TapeTestCase "0n1n: reject blank" zeroNoneNMachine "00.11" False ["blank character"]
    , TapeTestCase "0n1n: reject invalid char '2'" zeroNoneNMachine "0021" False ["Invalid characters", "2"]
    , TapeTestCase "0n1n: reject invalid char 'x'" zeroNoneNMachine "00x11" False ["Invalid characters", "x"]
    , TapeTestCase "0n1n: empty tape" zeroNoneNMachine "" True []
    
    -- 0^2n Tests
    , TapeTestCase "02n: valid input 0000" zeroTwoNMachine "0000" True []
    , TapeTestCase "02n: valid input 00" zeroTwoNMachine "00" True []
    , TapeTestCase "02n: reject blank" zeroTwoNMachine "00.0" False ["blank character"]
    , TapeTestCase "02n: reject invalid char '1'" zeroTwoNMachine "0010" False ["Invalid characters", "1"]
    , TapeTestCase "02n: reject invalid char 'z'" zeroTwoNMachine "00z0" False ["Invalid characters", "z"]
    , TapeTestCase "02n: empty tape" zeroTwoNMachine "" True []
    
    -- Palindrome Tests
    , TapeTestCase "Palindrome: valid input 0110" palindromeMachine "0110" True []
    , TapeTestCase "Palindrome: valid input 101" palindromeMachine "101" True []
    , TapeTestCase "Palindrome: reject blank" palindromeMachine "01.10" False ["blank character"]
    , TapeTestCase "Palindrome: reject invalid char '2'" palindromeMachine "0120" False ["Invalid characters", "2"]
    , TapeTestCase "Palindrome: reject invalid char 'p'" palindromeMachine "01p10" False ["Invalid characters", "p"]
    , TapeTestCase "Palindrome: empty tape" palindromeMachine "" True []
    ]

-- ====================
-- Test Generation
-- ====================

makeTest :: TapeTestCase -> TestTree
makeTest tc = testCase (tcName tc) $
    case validateTape (tcMachine tc) (tcTape tc) of
        Right () -> 
            if tcShouldSucceed tc
                then return ()
                else assertFailure $ "Expected error but validation succeeded for: " ++ tcTape tc
        Left msg ->
            if tcShouldSucceed tc
                then assertFailure $ "Expected success but got error: " ++ msg
                else
                    mapM_ (\keyword -> 
                        assertBool ("Error message should contain '" ++ keyword ++ "'. Got: " ++ msg)
                                   (keyword `isInfixOf` msg)
                    ) (tcExpectedInMessage tc)

-- ====================
-- Main Test Tree
-- ====================

tests :: TestTree
tests = testGroup "Tape Validation Tests" (map makeTest allTestCases)

-- ====================
-- Helper Functions
-- ====================

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs