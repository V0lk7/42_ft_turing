{-# LANGUAGE OverloadedStrings #-}

module JsonParserTest (tests) where

import Control.Exception (SomeException, try)
import Json_Parser qualified as JP
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

-- Chemins vers les fixtures
validFixtures :: FilePath
validFixtures = "test" </> "jsonFiles" </> "valid"

invalidFixtures :: FilePath
invalidFixtures = "test" </> "jsonFiles" </> "invalid"

tests :: TestTree
tests =
  testGroup
    "JSON Parser Tests"
    [ validMachineTests,
      invalidMachineTests
    ]

-- Tests pour les machines valides
validMachineTests :: TestTree
validMachineTests =
  testGroup
    "Valid Machine Tests"
    [ testCase "Parse well-formatted machine" $ do
        machine <- JP.parseMachineFile (validFixtures </> "unary_addition.json")
        JP.mName machine @?= "unary_format_addition"
        JP.mInitial machine @?= "scan_left_operand"
        length (JP.mStates machine) @?= 4
        length (JP.mAlphabet machine) @?= 3
        JP.mBlank machine @?= "."
        JP.mFinals machine @?= ["HALT"]
        -- Vérifie que les transitions existent pour les bons états
        length (JP.mTransitions machine) @?= 3
    ]

-- Tests pour les machines invalides (doivent échouer)
invalidMachineTests :: TestTree
invalidMachineTests =
  testGroup
    "Invalid Machine Tests"
    [ testGroup
        "Basic Structure Errors"
        [ testCase "Empty JSON object should fail" $
            assertParseFailure "empty_json_object.json",
          testCase "Not a JSON object should fail" $
            assertParseFailure "not_json_object.json",
          testCase "Wrong key in JSON should fail" $
            assertParseFailure "wrong_key.json",
          testCase "Wrong format file should fail" $
            assertParseFailure "wrong_format.txt",
          testCase "file does not exist should fail" $
            assertParseFailure "file_does_not_exist.json"
        ],
      testGroup
        "Name Field Errors"
        [ testCase "Missing name key should fail" $
            assertParseFailure "name_missing_key.json",
          testCase "Wrong name type should fail" $
            assertParseFailure "name_wrong_type.json"
        ],
      testGroup
        "Alphabet Field Errors"
        [ testCase "Missing alphabet key should fail" $
            assertParseFailure "alphabet_missing_key.json",
          testCase "Wrong alphabet type should fail" $
            assertParseFailure "alphabet_wrong_type.json",
          testCase "Empty alphabet list should fail" $
            assertParseFailure "alphabet_empty_list.json"
        ],
      testGroup
        "Blank Field Errors"
        [ testCase "Missing blank key should fail" $
            assertParseFailure "blank_missing_key.json",
          testCase "Wrong blank type should fail" $
            assertParseFailure "blank_wrong_type.json"
        ],
      testGroup
        "States Field Errors"
        [ testCase "Missing states key should fail" $
            assertParseFailure "state_missing_key.json",
          testCase "Wrong states type should fail" $
            assertParseFailure "state_wrong_type.json",
          testCase "Empty states list should fail" $
            assertParseFailure "state_list_empty.json"
        ],
      testGroup
        "Initial State Errors"
        [ testCase "Missing initial key should fail" $
            assertParseFailure "initial_missing_key.json",
          testCase "Wrong initial type should fail" $
            assertParseFailure "initial_wrong_type.json",
          testCase "Initial not in states should fail" $
            assertParseFailure "initial_not_in_state.json"
        ],
      testGroup
        "Final States Errors"
        [ testCase "Missing finals key should fail" $
            assertParseFailure "finals_missing_key.json",
          testCase "Wrong finals type should fail" $
            assertParseFailure "finals_wrong_type.json",
          testCase "Finals not in states should fail" $
            assertParseFailure "finals_not_in_state_list.json",
          testCase "Finals not in states (2) should fail" $
            assertParseFailure "finals_not_in_state_list_2.json"
        ],
      testGroup
        "Transitions Structure Errors"
        [ testCase "Missing transitions key should fail" $
            assertParseFailure "transitions_missing_key.json",
          testCase "Wrong transitions type should fail" $
            assertParseFailure "transitions_wrong_type.json",
          testCase "Empty transitions object should fail" $
            assertParseFailure "transitions_empty_object.json",
          testCase "Transitions with unknown state should fail" $
            assertParseFailure "transitions_wrong_state.json",
          testCase "Transitions missing one state should fail" $
            assertParseFailure "transitions_missing_one_state.json",
          testCase "Transition state empty should fail" $
            assertParseFailure "transitions_state_empty.json",
          testCase "Transition wrong type should fail" $
            assertParseFailure "transitions_state_wrong_type.json",
          testCase "Finals state in transitions should fail" $
            assertParseFailure "transitions_finals_state_in_it.json"
        ],
      testGroup
        "Transition Read Field Errors"
        [ testCase "Missing read key should fail" $
            assertParseFailure "transitions_state_read_missing_key.json",
          testCase "Wrong read type should fail" $
            assertParseFailure "transitions_state_read_wrong_type.json",
          testCase "Read not in alphabet should fail" $
            assertParseFailure "transitions_state_read_character_not_in_alphabet.json"
        ],
      testGroup
        "Transition To_State Field Errors"
        [ testCase "Missing to_state key should fail" $
            assertParseFailure "transitions_state_toState_missing_key.json",
          testCase "Wrong to_state type should fail" $
            assertParseFailure "transitions_state_toState_wrong_type.json",
          testCase "To_state nonexistent should fail" $
            assertParseFailure "transitions_state_toState_nonexistant.json"
        ],
      testGroup
        "Transition Write Field Errors"
        [ testCase "Missing write key should fail" $
            assertParseFailure "transitions_state_write_missing_key.json",
          testCase "Wrong write type should fail" $
            assertParseFailure "transitions_state_write_wrong_type.json",
          testCase "Write not in alphabet should fail" $
            assertParseFailure "transitions_state_write_character_not_in_alphabet.json"
        ],
      testGroup
        "Transition Action Field Errors"
        [ testCase "Missing action key should fail" $
            assertParseFailure "transitions_state_action_missing_key.json",
          testCase "Wrong action type should fail" $
            assertParseFailure "transitions_state_action_wrong_type.json",
          testCase "Unknown action should fail" $
            assertParseFailure "transitions_state_action_unknow.json"
        ]
    ]

-- Fonction utilitaire pour tester qu'un parsing échoue
assertParseFailure :: FilePath -> Assertion
assertParseFailure filename = do
  let path = invalidFixtures </> filename
  result <- try (JP.parseMachineFile path) :: IO (Either SomeException JP.Machine)
  case result of
    Left _ -> return () -- Le parsing a échoué comme attendu
    Right _ -> assertFailure $ "Expected parsing to fail for " ++ filename ++ ", but it succeeded"
