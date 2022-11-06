-- | ...
-- Module: Elea.Types.Eleaa
-- Description: Core Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Object.Phenomena (
    -- Space
    Abstraction (..), AbstractionId (..)
  , State
  , Arrow (..), ArrowId (..)
    -- Time
  , Mutation (..), Type (..)
  , Computer (..), Effects (..), Property (..)
    -- Agency
  , Story (..), StoryId (..), Event (..), EventId (..)
  , Agent (..), AgentId (..)
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
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM (
    empty
  , insertWith
  , findWithDefault
  )
import           Data.Text (Text)
import qualified Data.Text as T (drop, pack, toLower)
import           GHC.Generics

import qualified Elea.Atom.Phenomena as Atom

--------------------------------------------------------------------------------
-- COMPOSITION
--------------------------------------------------------------------------------

-- | Abstraction
data Abstraction = Abstraction {
    id     :: AbstractionId
  , states :: [State]
} deriving (Eq, Generic, Show)

instance ToJSON Abstraction

instance FromJSON Abstraction

-- | AbstractionId
newtype AbstractionId = AbstractionId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON AbstractionId where
  toJSON (AbstractionId s) = JSON.String s

instance FromJSON AbstractionId where
  parseJSON (JSON.String t) = return $ AbstractionId t
  parseJSON invalid         =
    prependFailure "parsing AbstractionId failed, "
        (typeMismatch "String" invalid)


-- | State
newtype State = State {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON State where
  toJSON (State s) = JSON.String s

instance FromJSON State where
  parseJSON (JSON.String t) = return $ State t
  parseJSON invalid         =
    prependFailure "parsing State failed, "
        (typeMismatch "String" invalid)


-- | Arrow
-- Talk about different types of arrows. Experience arrow
-- An arrow is just a relation
data Arrow = Arrow {
    id           :: ArrowId
  , initStateRef :: State
  , termStateRef :: State
} deriving (Eq, Generic, Show)

instance ToJSON Arrow where
  toJSON arrow = JSON.object [
      "id"             .= arrow.id 
    , "init_state_ref" .= arrow.initStateRef
    , "term_state_ref" .= arrow.termStateRef
    ]

instance FromJSON Arrow

-- | ArrowId
newtype ArrowId = ArrowId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON ArrowId where
  toJSON (ArrowId s) = JSON.String s

instance FromJSON ArrowId where
  parseJSON (JSON.String t) = return $ ArrowId t
  parseJSON invalid         =
    prependFailure "parsing ArrowId failed, "
        (typeMismatch "String" invalid)


-- | Reference
data Reference = Reference {
    stateId       :: State
  , arrowId       :: ArrowId
} deriving (Eq, Generic, Show)

instance ToJSON Reference

instance FromJSON Reference


--------------------------------------------------------------------------------
-- CAUSALITY
--------------------------------------------------------------------------------

-- | Mutation
data Mutation = 
    Addition Arrow
  | Subtraction Reference
  deriving (Eq, Generic, Show)


-- | Type
data Type = Type {
    id     :: Text
  , change :: [Mutation]
} deriving (Eq, Generic, Show)

-- | TypeId
-- type as set of mutations sticks to original idea from last summer
-- how would this interact with database ideas?
newtype TypeId = TypeId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON TypeId where
  toJSON (TypeId s) = JSON.String s

instance FromJSON TypeId where
  parseJSON (JSON.String t) = return $ TypeId t
  parseJSON invalid         =
    prependFailure "parsing TypeId failed, "
        (typeMismatch "String" invalid)


-- | Computer
-- cannot prove a computer
-- that's why stories only prove abstractions
-- stories cannot prove computers because of the halting problem
-- TODO can we just assume the abstraction?
data Computer = Computer {
    id         :: Text
    -- Refernce
  , reference  :: Reference
    -- Type
  , type_      :: Type 
    -- Effects
  , effects    :: Effects
     --properties are computers which represent what the
     --actual computer will do. they are like promises, 
     --promises that the computer will have a certain behavior
     --evidence?
  , properties :: [Property]
} deriving (Eq, Generic, Show)


-- | Property
data Property = Property {
    id    :: PropertyId
  , story :: StoryId
} deriving (Eq, Generic, Show)


-- | PropertyId
-- type as set of mutations sticks to original idea from last summer
-- how would this interact with database ideas?
newtype PropertyId = PropertyId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON PropertyId where
  toJSON (PropertyId s) = JSON.String s

instance FromJSON PropertyId where
  parseJSON (JSON.String t) = return $ PropertyId t
  parseJSON invalid         =
    prependFailure "parsing PropertyId failed, "
        (typeMismatch "String" invalid)


-- | Effects
data Effects = Effects {
    before :: [StoryId] 
  , after  :: [StoryId]
} deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- AGENCY
--------------------------------------------------------------------------------

-- | Story
data Story = Story {
    id     :: StoryId
  , events :: [Event]
} deriving (Eq, Generic, Show)


-- | StoryId
newtype StoryId = StoryId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON StoryId where
  toJSON (StoryId s) = JSON.String s

instance FromJSON StoryId where
  parseJSON (JSON.String t) = return $ StoryId t
  parseJSON invalid         =
    prependFailure "parsing StoryId failed, "
        (typeMismatch "String" invalid)


-- | Event
data Event = Event {
    id     :: EventId
  , events :: Computer
} deriving (Eq, Generic, Show)


-- | EventId
newtype EventId = EventId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON EventId where
  toJSON (EventId s) = JSON.String s

instance FromJSON EventId where
  parseJSON (JSON.String t) = return $ EventId t
  parseJSON invalid         =
    prependFailure "parsing EventId failed, "
        (typeMismatch "String" invalid)


-- | Aent
data Agent = Agent {
  events  :: [Event]
} deriving (Eq, Generic, Show)


-- | AgentId
newtype AgentId = AgentId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON AgentId where
  toJSON (AgentId s) = JSON.String s

instance FromJSON AgentId where
  parseJSON (JSON.String t) = return $ AgentId t
  parseJSON invalid         =
    prependFailure "parsing AgentId failed, "
        (typeMismatch "String" invalid)

