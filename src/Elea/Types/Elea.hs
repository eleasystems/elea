-- | ...
-- Module: Elea.Types.Eleaa
-- Description: Core Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Types.Elea (
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
-- REALITY
--------------------------------------------------------------------------------

-- | Phenomena
data Phenomena = 
    PhenomenaComposition
  | PhenomenaCasuality
  | PhenomenaConsensus
  deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- COMPOSITION
-- space
--------------------------------------------------------------------------------

-- | Abstraction
newtype Abstraction = Abstraction {
  states :: [State]
} deriving (Eq, Generic, Show)

instance ToJSON Abstraction

instance FromJSON Abstraction

instance Semigroup Abstraction where
  Abstraction states1 <> Abstraction states2 = Abstraction (states1 <> states2)

-- | AbstractionId
newtype AbstractionId = AbstractionId {
  val :: Text
} deriving (Eq, Generic, Show)

instance ToJSON AbstractionId where
  toJSON (AbstractionId s) = JSON.String s

instance FromJSON AbstractionId where
  parseJSON (JSON.String t) = return $ AbstractionId t
  parseJSON invalid         =
    prependFailure "parsing AbstractionId failed, "
        (typeMismatch "String" invalid)


-- | State
data State = State {
    id     :: StateId
  , arrows :: [Arrow]
} deriving (Eq, Generic, Show)

instance ToJSON State

instance FromJSON State

-- | StateId
newtype StateId = StateId {
  val :: Text
} deriving (Eq, Generic, Show)

instance ToJSON StateId where
  toJSON (StateId s) = JSON.String s

instance FromJSON StateId where
  parseJSON (JSON.String t) = return $ StateId t
  parseJSON invalid         =
    prependFailure "parsing StateId failed, "
        (typeMismatch "String" invalid)


-- | Arrow
-- Talk about different types of arrows. Experience arrow
-- An arrow is just a relation
data Arrow = Arrow {
    id           :: ArrowId
  , initStateRef :: StateReference
  , termStateRef :: StateReference
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
  val :: Text
} deriving (Eq, Generic, Show)

instance ToJSON ArrowId where
  toJSON (ArrowId s) = JSON.String s

instance FromJSON ArrowId where
  parseJSON (JSON.String t) = return $ ArrowId t
  parseJSON invalid         =
    prependFailure "parsing ArrowId failed, "
        (typeMismatch "String" invalid)


-- | Reference
data Reference = 
    ReferenceAbstraction AbstractionReference
  | ReferenceState StateReference
  | ReferenceArrow ArrowReference
  deriving (Eq, Generic, Show)

-- | AbstractionReference Reference
newtype AbstractionReference = AbstractionReference {
  abstractionId :: AbstractionId
} deriving (Eq, Generic, Show)

-- | State Reference
data StateReference = StateReference {
    abstractionId :: AbstractionId
  , stateId       :: StateId
} deriving (Eq, Generic, Show)

instance ToJSON StateReference

instance FromJSON StateReference

-- | Arrow Reference
data ArrowReference = ArrowReference {
    abstractionId :: AbstractionId
  , stateId       :: StateId
  , arrowId       :: ArrowId
} deriving (Eq, Generic, Show)


-- | Consciousness
--data Consciousness = 
    --LocalConsciousness
  -- | SharedConsciousness

--------------------------------------------------------------------------------
-- CAUSALITY
-- time
--------------------------------------------------------------------------------

-- | Mutation
data Mutation = 
    Addition Arrow
  | Subtraction ArrowReference
  deriving (Eq, Generic, Show)


-- | Computer
-- cannot prove a computer
-- that's why stories only prove abstractions
-- stories cannot prove computers because of the halting problem
data Computer = Computer {
    id         :: Text
    -- Types
  , type_      :: Type 
    -- Effects
  , effects    :: Effects
    -- properties are computers which represent what the
    -- actual computer will do. they are like promises, 
    -- promises that the computer will have a certain behavior
    -- evidence?
  , properties :: [Property]
} deriving (Eq, Generic, Show)


-- | Type
data Type = Type {
    id     :: Text
  , domain :: Proof
  , time   :: ArrowReference
  , change :: [Mutation]
} deriving (Eq, Generic, Show)


-- | Property
data Property = Property {
    id    :: Text
  , story :: Story
} deriving (Eq, Generic, Show)


-- | Effects
data Effects = Effects {
    before :: [Proof] 
  , after  :: [Proof]
} deriving (Eq, Generic, Show)


--------------------------------------------------------------------------------
-- CONSENSUS
-- control flow
--------------------------------------------------------------------------------

-- | Story
newtype Story = Story {
  events  :: [Event]
} deriving (Eq, Generic, Show)


-- computation is a story that already happened and is certified as such


-- | Event
data Event = Event {
    conditions  :: [Effect]
  , computer    :: [Computer]
  , sideEffects :: [Effect]
} deriving (Eq, Generic, Show)


-- | Effect
data Effect = Effect {
    abstractionProof :: Proof
  , initState        :: StateId
  , endState         :: StateId
} deriving (Eq, Generic, Show)


-- | Proof
data Proof = Proof {
    constructor :: Story 
  , conditions  :: [Effect]
  , evidence    :: [Story] 
} deriving (Eq, Generic, Show)

-- | ProofId
newtype ProofId = ProofId {
  val :: Text
} deriving (Eq, Generic, Show)

instance ToJSON ProofId where
  toJSON (ProofId s) = JSON.String s

instance FromJSON ProofId where
  parseJSON (JSON.String t) = return $ ProofId t
  parseJSON invalid         =
    prependFailure "parsing ProofId failed, "
        (typeMismatch "String" invalid)



-- | Program
-- programs are identity computers for elea?
newtype Program = Program {
  proofs :: [Proof]
} deriving (Eq, Generic, Show)



-- todo
-- programs / identity 
-- parts of consciousness

