-- | ...
-- Module: Base
-- Description : Base Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Base (
    Abstraction (..)
  , Arrow (..), ArrowId (..), ArrowName (..)
  , Computer (..), existence
  , Def (..)
  , State (..), StateId (..)
  , Story (..)
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


--------------------------------------------------------------------------------
-- | Def
--------------------------------------------------------------------------------

data Def = 
    DefAbstraction
  | DefArrow
  | DefComputer
  | DefIdentity
  | DefMutation
  | DefState
  | DefStory
  deriving (Eq, Generic, Show)

instance Hashable Def

instance ToJSON Def where
  toJSON object = JSON.String $ T.drop 6 $ T.toLower $ T.pack $ show object

instance FromJSON Def where
  parseJSON (JSON.String s) = case s of
    "abstraction" -> return DefAbstraction
    "state"       -> return DefState
    "arrow"       -> return DefArrow
    "mutation"    -> return DefMutation
    "identity"    -> return DefIdentity
    "computer"    -> return DefComputer
    _             -> fail "Def must be one of: abstraction, arrow, computer, identity, mutation, or state"
  parseJSON _ = fail "Expected one of: abstraction, state, arrow, mutation, identity, computer"

--------------------------------------------------------------------------------
-- | Abstraction
--------------------------------------------------------------------------------

-- | Abstraction
newtype Abstraction = Abstraction {
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

instance ToJSON StateId where
  toJSON (StateId s) = JSON.String s

instance FromJSON StateId where
  parseJSON (JSON.String t) = return $ StateId t
  parseJSON invalid         =
    prependFailure "parsing StateId failed, "
        (typeMismatch "String" invalid)


--------------------------------------------------------------------------------
-- | Arrow
--------------------------------------------------------------------------------

-- | Arrow
-- Talk about different types of arrows. Experience arrow
-- An arrow is just a relation
data Arrow = Arrow {
    id        :: ArrowId
  , name      :: ArrowName
  , initState :: StateId
  , termState :: StateId
  , mutations :: [Mutation]
} deriving (Eq, Generic, Show)

instance ToJSON Arrow where
  toJSON arrow = JSON.object [
      "id"         .= arrow.id
    , "name"       .= arrow.name
    , "init_state" .= arrow.initState
    , "term_state" .= arrow.termState
    , "mutations"  .= arrow.mutations
    ]

instance FromJSON Arrow


-- | ArrowId
newtype ArrowId = ArrowId {
  getArrowId :: Text
} deriving (Eq, Generic, Show)

instance ToJSON ArrowId where
  toJSON (ArrowId s) = JSON.String s

instance FromJSON ArrowId where
  parseJSON (JSON.String t) = return $ ArrowId t
  parseJSON invalid         =
    prependFailure "parsing ArrowId failed, "
        (typeMismatch "String" invalid)


-- | ArrowName
newtype ArrowName = ArrowName {
  getArrowName :: Text
} deriving (Eq, Generic, Show)

instance ToJSON ArrowName where
  toJSON (ArrowName s) = JSON.String s

instance FromJSON ArrowName where
  parseJSON (JSON.String t) = return $ ArrowName t
  parseJSON invalid         =
    prependFailure "parsing ArrowName failed, "
        (typeMismatch "String" invalid)


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

instance ToJSON Mutation where
  toJSON mut = JSON.String $ T.toLower $ T.pack $ show mut

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
  , forDef    :: Def
  , uri       :: Text
  , resources :: [AbstractionId]
} deriving (Eq, Generic, Show)

instance Hashable Computer

instance ToJSON Computer where
  toJSON computer = JSON.object [
      "name"       .= computer.name
    , "for_object" .= computer.forDef
    , "uri"        .= computer.uri
    , "resources"  .= computer.resources
    ]

instance FromJSON Computer where
  parseJSON = JSON.withObject "Computer" $ \v -> Computer
    <$> v .: "name"
    <*> v .: "for_object"
    <*> v .: "uri"
    <*> v .: "resources"


existence :: Computer
existence  = Computer {
    name      = "Existence"
  , forDef    = DefComputer
  , uri       = ""
  , resources = []
}

--------------------------------------------------------------------------------
-- | Story
--------------------------------------------------------------------------------

-- | Story
data Story = Story {
    id     :: Text
  , name   :: Text
  , arrows :: [Arrow]
} deriving (Generic, Show)

instance ToJSON Story where
  toJSON story = JSON.object [
      "name"   .= story.name
    , "id"     .= story.id
    , "arrows" .= story.arrows
    ]

instance FromJSON Story

