module TapeValidator (validateTape) where

import qualified Json_Parser as JP
import qualified Data.Text as T

validateTape :: JP.Machine -> String -> Either String ()
validateTape machine tape =
    let alphabet = JP.mAlphabet machine
        blank = JP.mBlank machine
        alphabetChars = map T.head alphabet
        alphabetWithoutBlank = filter (/= T.head blank) alphabetChars
        
        containsBlank = T.isInfixOf blank (T.pack tape)
        
        invalidChars = filter (`notElem` alphabetWithoutBlank) tape
        
    in if null tape
        then Left "Error: Input tape cannot be empty"
        else if containsBlank
            then Left $ "Error: Input tape cannot contain the blank character '"
                    ++ T.unpack blank ++ "'"
        else if not (null invalidChars)
            then Left $ "Error: Invalid characters in tape: " ++ invalidChars
                        ++ "\nAllowed characters: " ++ alphabetWithoutBlank
            else Right ()