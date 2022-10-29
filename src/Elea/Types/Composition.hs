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



