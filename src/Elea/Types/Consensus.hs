-- | ...
-- Module: Elea.Types.Derived
-- Description : Elea derived types (types dependent on the Base types)
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Types.Consensus (
    Story (..)
  ) where


import           Data.Aeson (
    ToJSON (..), (.=)
  , FromJSON (..), (.:)
  )
import qualified Data.Aeson as JSON (
    Value (..)
  , object, withObject
  )
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T (drop, pack, toLower)
import           GHC.Generics

import           Elea.Types.Causality (Computer)


-- | Story
data Story = Story {
    id        :: StoryId
  , computers :: [Computer]
} deriving (Generic, Show)

instance ToJSON Story where
  toJSON story = JSON.object [
      "id"        .= story.id
      "computers" .= story.computers
    ]

instance FromJSON Story

-- | StoryId
newtype StoryId = StoryId {
  val :: Text
} deriving (Eq, Generic, Show)

instance ToJSON StoryId where
  toJSON (StoryId s) = JSON.String s

instance FromJSON StoryId where
  parseJSON (JSON.String t) = return $ StoryId t
  parseJSON invalid         =
    prependFailure "parsing StoryId failed, "
        (typeMismatch "String" invalid)


-- | Server
data Server = Server {
    id      :: ServerId
  , stories :: [Story]
} deriving (Generic, Show)

instance ToJSON Server where
  toJSON server = JSON.object [
      "id"      .= server.id
    , "stories" .= server.stories
    ]

instance FromJSON Server

-- | ServerId
newtype ServerId = ServerId {
  val :: Text
} deriving (Eq, Generic, Show)

instance ToJSON ServerId where
  toJSON (ServerId s) = JSON.String s

instance FromJSON ServerId where
  parseJSON (JSON.String t) = return $ ServerId t
  parseJSON invalid         =
    prependFailure "parsing ServerId failed, "
        (typeMismatch "String" invalid)


-- | Proof
data Proof = Proof {
    constructor :: Story
  , evidenece   :: [Story]
}
