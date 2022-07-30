-- | ...
-- Module: Base
-- Description : Base Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Base (
    Abstraction (..)
  , Arrow (..)
  , Computer (..), existence
  , Object (..)
  , State (..)
  , Story (..)
  ) where


import           Data.Aeson (
    ToJSON (..), (.=)
  , FromJSON (..), (.:), withObject
  )
import qualified Data.Aeson as JSON (
    Value (..)
  , object
  )
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T (drop, pack, toLower)
import           GHC.Generics


--------------------------------------------------------------------------------
-- | Object
--------------------------------------------------------------------------------

data Object = 
    ObjectAbstraction
  | ObjectArrow
  | ObjectComputer
  | ObjectIdentity
  | ObjectMutation
  | ObjectState
  | ObjectStory
  deriving (Eq, Generic, Show)

instance Hashable Object

instance ToJSON Object where
  toJSON object = JSON.String $ T.drop 6 $ T.toLower $ T.pack $ show object

instance FromJSON Object where
  parseJSON (JSON.String s) = case s of
    "abstraction" -> return ObjectAbstraction
    "state"       -> return ObjectState
    "arrow"       -> return ObjectArrow
    "mutation"    -> return ObjectMutation
    "identity"    -> return ObjectIdentity
    "computer"    -> return ObjectComputer
  parseJSON _ = fail "Expected one of: abstraction, state, arrow, mutation, identity, computer"

--------------------------------------------------------------------------------
-- | Abstraction
--------------------------------------------------------------------------------

-- | Abstraction
data Abstraction = Abstraction {
  states :: [State]
} deriving (Eq, Generic, Show)

instance FromJSON Abstraction


-- | AbstractionId
newtype AbstractionId = AbstractionId {
  getAbstractionId :: Text
} deriving (Eq, Generic, Show)

instance Hashable AbstractionId

instance ToJSON AbstractionId

instance FromJSON AbstractionId

--------------------------------------------------------------------------------
-- | State
--------------------------------------------------------------------------------

-- | State
data State = State {
    id     :: StateId
  , arrows :: [ArrowId]
} deriving (Eq, Generic, Show)

instance FromJSON State


-- | StateId
newtype StateId = StateId {
  getStateId :: Text
} deriving (Eq, Generic, Show)

instance FromJSON StateId

--------------------------------------------------------------------------------
-- | Arrow
--------------------------------------------------------------------------------

-- | Arrow
-- Talk about different types of arrows. Experience arrow
-- An arrow is just a relation
data Arrow = Arrow {
    id        :: ArrowId
  , initState :: StateId
  , termState :: StateId
  , mutations :: [Mutation]
} deriving (Eq, Generic, Show)

instance FromJSON Arrow


-- | ArrowId
newtype ArrowId = ArrowId {
  getArrowId :: Text
} deriving (Eq, Generic, Show)

instance FromJSON ArrowId

--------------------------------------------------------------------------------
-- | Mutation
--------------------------------------------------------------------------------

-- | Mutation
data Mutation = 
    Contain
  | Divide
  | Connect
  | Transition
  deriving (Eq, Generic, Show)

instance FromJSON Mutation

--------------------------------------------------------------------------------
-- | Identity
--------------------------------------------------------------------------------

-- | Identity
data Identity = 
    SensoryIdentity IdentitySensory
  | MathemticalIdentity IdentityMathematical
  deriving (Generic, Show)

instance FromJSON Identity


-- | Sensory identitis
data IdentitySensory = 
    SightIdentity IdentitySensorySight 
  | SoundIdentity
  | FeelIdentity
  | SmellIdentity
  | TasteIdentity
  deriving (Generic, Show)

instance FromJSON IdentitySensory


-- | Visual (sight) identities
data IdentitySensorySight = 
  -- All (not blind) humans rely heavily on image recognition
    ImageStreamIdentity
  -- A visually-processed language of some form is ubiquitous in 
  -- human consciousness
  -- Overlaps with ImageStream, but depends on the abstraction that is being
  -- used (ie. what the person is focusing on, when you are reading you don't
  -- notice the surrounding contet)
  | SymbolStreamIdentity
  deriving (Generic, Show)

instance FromJSON IdentitySensorySight


-- | Mathemtical identity
data IdentityMathematical = 
    AbstractionIdentity AbstractionId
  | StateIdentity       AbstractionId StateId
  | ArrowIdentity       AbstractionId StateId ArrowId
  deriving (Generic, Show)

instance FromJSON IdentityMathematical

--------------------------------------------------------------------------------
-- | Computer
--------------------------------------------------------------------------------

data Computer = Computer {
    name      :: Text
  , forObject :: Object
  , uri       :: Text
  , resources :: [AbstractionId]
} deriving (Eq, Generic, Show)

instance Hashable Computer

instance ToJSON Computer where
  toJSON computer = JSON.object $ [
      "name"       .= computer.name
    , "for_object" .= computer.forObject
    , "uri"        .= computer.uri
    , "resources"  .= computer.resources
    ]

instance FromJSON Computer where
  parseJSON = withObject "Computer" $ \v -> Computer
    <$> v .: "name"
    <*> v .: "for_object"
    <*> v .: "uri"
    <*> v .: "resources"


existence :: Computer
existence  = Computer {
    name      = "Existence"
  , forObject = ObjectComputer
  , uri       = ""
  , resources = []
}

--------------------------------------------------------------------------------
-- | Story
--------------------------------------------------------------------------------

-- | Story
data Story = Story {
    name :: Text
  , arrows :: [Arrow]
} deriving (Generic, Show)

instance FromJSON Story

