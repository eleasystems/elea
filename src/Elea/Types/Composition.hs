-- | ...
-- Module: Composition / Structure
-- Description : Base Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Types.Composition (
    Program (..), ProgramId (..)
  , Reference (..), possibility, moment, affordance
  , Structure (..)
  , Abstraction (..), AbstractionId (..)
  , Arrow (..), ArrowId (..)
  , State (..), StateId (..)
  , Mutation (..)
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
import           Data.ByteString (ByteString)
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T (drop, pack, toLower)
import           GHC.Generics


-- | Program
-- An intention built around particular structures
data Program = Program {
    id           :: ProgramId
  , abstractions :: [Abstraction]
} deriving (Eq, Generic, Show)

instance ToJSON Program

instance FromJSON Program

-- | ProgramId
newtype ProgramId = ProgramId {
  val :: Text
} deriving (Eq, Generic, Show)

instance ToJSON ProgramId where
  toJSON (ProgramId s) = JSON.String s

instance FromJSON ProgramId where
  parseJSON (JSON.String t) = return $ ProgramId t
  parseJSON invalid         =
    prependFailure "parsing ProgramId failed, "
        (typeMismatch "String" invalid)


-- | Reference
data Reference = 
    ReferenceAbstraction AbstractionReference
  | ReferenceState StateReference
  | ReferenceArrow ArrowReference
  
newtype AbstractionReference = AbstractionReference AbstractionId

data StateReference = StateReference AbstractionId StateId

data ArrowReference = ArrowReference AbstractionId StateId ArrowId

-- | Structure
struct :: Program -> Reference -> Structure
struct p (ReferenceAbstraction (AbstractionReference absId           )) = 
  StructureAbstraction absId
struct p (ReferenceState       (StateReference       absId stId      )) = 
  StructureState p
struct p (ReferenceArrow       (ArrowReference       absId stId arrId)) = 
  StructureArrow p

-- | Possibility
possibility :: AbstractionId -> Reference
possibility = RefAbstraction

-- | Moment
moment :: AbstractionId -> StateId -> Reference
moment = RefState

-- | Affordance
affordance :: AbstractionId -> StateId -> ArrowId -> Reference
affordance = RefArrow


-- | Structure
data Structure = 
    StructureAbstraction Abstraction
  | StructureState State
  | StructureArrow Arrow
  deriving (Eq, Generic, Show)

--instance Hashable Structure

--instance ToJSON Structure where
  --toJSON object = JSON.String $ T.drop 6 $ T.toLower $ T.pack $ show object

--instance FromJSON Structure where
  --parseJSON (JSON.String s) = case s of
    --"abstraction" -> return StructureAbstraction
    --"state"       -> return StructureState
    --"arrow"       -> return StructureArrow
    --_             -> fail "Struct must be one of: abstraction, state, or arrow"
  --parseJSON invalid         =
    --prependFailure "parsing Struct failed, "
        --(typeMismatch "String" invalid)


-- | Abstraction
newtype Abstraction = Abstraction {
  states :: [State]
} deriving (Eq, Generic, Show)

instance ToJSON Abstraction

instance FromJSON Abstraction

instance Semigroup Abstraction where
  Abstraction states1 <> Abstraction states2 = Abstraction (states1 <> states2)

-- | ProgramId
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
    initStateId :: StateId
  , termStateId :: StateId
  , mutations   :: [Mutation]
} deriving (Eq, Generic, Show)

instance ToJSON Arrow where
  toJSON arrow = JSON.object [
      "init_state_id" .= arrow.initStateId
    , "term_state_id" .= arrow.termStateId
    , "mutations"     .= arrow.mutations
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


