module TapeValidator (validateTape) where

import qualified Json_Parser as JP
import qualified Data.Text as T

validateTape :: JP.Machine -> String -> Either String ()
validateTape machine tape =
    let alphabet = JP.mAlphabet machine
        blank = JP.mBlank machine
        blankChar = T.head blank
        alphabetChars = map T.head alphabet
        alphabetWithoutBlank = filter (/= blankChar) alphabetChars
        
        containsBlank = blankChar `elem` tape
        
        invalidChars = filter (`notElem` alphabetWithoutBlank) tape
        
    in if containsBlank
        then Left $ "Error: Input tape cannot contain the blank character '"
                    ++ [blankChar] ++ "'"
        else if not (null invalidChars)
            then Left $ "Error: Invalid characters in tape: " ++ invalidChars
                        ++ "\nAllowed characters: " ++ alphabetWithoutBlank
            else Right ()