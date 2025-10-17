{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Json_Parser
    ( Machine(..)
    , Transition(..)
    , Action(..)
    , parseMachineFile
    ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
    ( FromJSON(parseJSON)
    , (.:)
    , withObject
    , eitherDecode
    , Value(Object))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (toText)
import qualified Data.Map.Strict as M
import System.Exit (die)

-- Action for transitions
data Action = RIGHT | LEFT
    deriving (Show, Eq, Generic)

-- Transition for the machine
data Transition = Transition
    { trRead    ::  Text
    , trTo      ::  Text
    , trWrite   ::  Text
    , trAct     ::  Action
    } deriving (Show, Eq, Generic)

-- Machine
data Machine = Machine
    { mName         :: Text
    , mAlphabet     :: [Text]
    , mBlank        :: Text
    , mStates       :: [Text]
    , mInitial      :: Text
    , mFinals       :: [Text]
    , mTransitions  :: M.Map Text [Transition]
    } deriving (Show, Eq, Generic)

-- Parse Action
parseActionText :: Text -> Parser Action
parseActionText t = case T.toUpper (T.strip t) of
                        "RIGHT" ->  return RIGHT
                        "LEFT"  ->  return LEFT
                        other   ->  fail $ "Invalid action: action can only be \"RIGHT\" or \"LEFT\", found: "
                                                                ++ T.unpack other

-- Intermediate Transition struct to read each JSON transitions
-- { "read": "...", "to_state": "...", "write": "...", "action": "..."}
data TransitionRaw = TransitionRaw
    { rawRead   ::  Text
    , rawTo     ::  Text
    , rawWrite  ::  Text
    , rawAct    ::  Text
    } deriving (Show, Generic)

instance FromJSON TransitionRaw where
    parseJSON = withObject "Transition" $ \o -> do
        r <- o .: "read"
        t <- o .: "to_state"
        w <- o .: "write"
        a <- o .: "action"
        return $ TransitionRaw r t w a

-- parse Transitions
parseTransitionsObject :: Value -> Parser (M.Map Text [TransitionRaw])
parseTransitionsObject (Object km) = do
    let kvs :: [(Text, Value)]
        kvs = map (\(k, v) -> (toText k, v)) (KM.toList km)
    let decodePair (k, v) = (,) k <$> parseJSONList v
    pairs <- mapM decodePair kvs
    return $ M.fromList pairs
    where
        parseJSONList :: Value -> Parser [TransitionRaw]
        parseJSONList = parseJSON
parseTransitionsObject _ = fail "invalid transitions field: transitions field need to be a list of transitions"

-- Machine validation
instance FromJSON Machine where
    parseJSON = withObject "Machine" $ \o -> do
        name        <- o .: "name"
        alphabet    <- o .: "alphabet"
        blank       <- o .: "blank"
        states      <- o .: "states"
        initial     <- o .: "initial"
        finals      <- o .: "finals"
        transVal    <- o .: "transitions"
        transRawMap <- parseTransitionsObject transVal

        -- validation
        -- non empty transitions
        if M.null transRawMap
            then fail "Invalid machine: 'transitions' cannot be empty."
            else return ()

        -- no transition should be empty
        let emptyTransitionStates = [ k | (k, lst) <- M.toList transRawMap, null lst ]
        if null emptyTransitionStates
            then return ()
            else fail $ "Invalid machine: the following state(s) have an empty transition list: "
                                    ++ show (map T.unpack emptyTransitionStates)

        -- non empty states
        if null (states :: [Text])
            then fail "Invalid machine: 'states' must contain at least one state."
            else return ()

        -- initial in states
        if initial `elem` states
            then return ()
            else fail $ "Invalid machine: 'initial' (" ++ T.unpack initial ++ ") is not present in 'states'."

        -- finals not empty and in states
        if null (finals :: [Text])
            then fail "Invalid machine: 'finals' cannot be empty."
            else return ()
        let unknownFinals = filter (`notElem` states) finals
        if null unknownFinals
            then return ()
            else fail $ "Invalid machine: 'finals' contains unknown states: " ++ show (map T.unpack unknownFinals)

        -- blank in alphabet
        if blank `elem` alphabet
            then return ()
            else fail $ "Invalid machine: 'blank' (" ++ T.unpack blank ++ ") is not present in 'alphabet'."

        -- transitions key in states
        let transKeys = M.keys transRawMap
            unknownKeys = filter (`notElem` states) transKeys
        if null unknownKeys
            then return ()
            else fail $ "Invalid machine: 'transitions' contains undeclared states: " ++ show (map T.unpack unknownKeys)

        -- final states must not have transitions
        let finalsWithTransitions = filter (`elem` transKeys) finals
        if null finalsWithTransitions
            then return ()
            else fail $ "Invalid machine: final state(s) must not have transitions defined: "
                                    ++ show (map T.unpack finalsWithTransitions)

        -- every non final state must have a transition
        let nonFinalStates = filter (`notElem` finals) states
            missingTransitionStates = filter (`notElem` transKeys) nonFinalStates
        if null missingTransitionStates
            then return ()
            else fail $ "Invalid machine: missing transition(s) for non-final states: "
                                     ++ show (map T.unpack missingTransitionStates)

        -- transform TransitionRaw to Transition while validating transition components
        let validateAndConvert :: (Text, TransitionRaw) -> Parser Transition
            validateAndConvert (srcState, trRaw) = do
                -- check if to_state exist
                let toSt = rawTo trRaw
                if toSt `elem` states
                    then return ()
                    else fail $ "Invalid machine: transition from '" ++ T.unpack srcState
                                    ++ "' in to_state is not declared: " ++ T.unpack toSt
                -- check read and write in alphabet
                let r = rawRead trRaw
                    w = rawWrite trRaw
                if r `elem` alphabet
                    then return ()
                    else fail $ "Invalid machine: read symbol: " ++ T.unpack r ++ "' in transition '"
                                    ++ T.unpack srcState ++ "' not present in 'alphabet'."
                if w `elem` alphabet
                    then return ()
                    else fail $ "Invalid machine: write symbol: " ++ T.unpack w ++ "' in transition '"
                                    ++ T.unpack srcState ++ "' not present in 'alphabet'."
                -- parse action
                a <- parseActionText (rawAct trRaw)
                -- successful conversion
                return $ Transition r toSt w a

        -- for each state convert from TransitionRaw to [Transition]
        convertedPairs <- mapM (\(st, lst) -> do
                                    trs <- mapM (validateAndConvert . (st,)) lst
                                    return (st, trs)
                               )    (M.toList transRawMap)
        let transitionsMap = M.fromList convertedPairs

        -- everything fine: build machine
        return $ Machine name alphabet blank states initial finals transitionsMap

-- open file, parse and die if an error occurs
parseMachineFile :: FilePath -> IO Machine
parseMachineFile path = do
    contents <- BL.readFile path
    case eitherDecode contents of
        Left err -> die $ "Error: JSON parsing error in " ++ path ++ ": " ++ err
        Right m  -> return m
