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
    , (.:?)
    , withObject
    , eitherDecode
    , Value(Object))
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
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
