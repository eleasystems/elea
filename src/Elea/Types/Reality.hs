-- | ...
-- Module: Base
-- Description : Base Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Types.Reality (
    -- Phenomena
    Phenomena (..)
    -- Proofs
  , Proof (..)
  , AbstractionProof (..), StateProof (..), ArrowProof (..)
    -- Program
  , Program (..)
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


import Elea.Types.Composition (Structure)
import Elea.Types.Consensus (Story)


-- | Phenomena
data Phenomena = 
    -- Containment
    -- Also known as Addition, Composition
    Containment AbstractionProof
    -- Order
    -- Also known as Division, Casuality
  | Order AbstractionProof StateProof
    -- Binding
    -- Also known as Multiplication, Consensus
  | Binding StateProof ArrowProof
  deriving (Eq, Generic, Show)


-- | Proof
data Proof = 
    ProofAbstraction AbstractionProof
  | ProofState StateProof
  | ProofArrow ArrowProof
  deriving (Eq, Generic, Show)

-- | Is Proof
instance IsProof a where
  story :: a -> Story 

-- | Proof: Abstraction
data AbstractionProof = AbstractionProof {
    abstraction :: Abstraction
  , story       :: Story
} deriving (Eq, Generic, Show)

-- | Proof: State
data StateProof = StateProof {
    state :: State
  , story :: Story
} deriving (Eq, Generic, Show)

-- | Proof: Arrow
data ArrowProof = ArrowProof {
    state :: Arrow
  , story :: Story
} deriving (Eq, Generic, Show)


-- | Program
newtype Program = Program {
  proofs :: [Proof]
} deriving (Eq, Generic, Show)


-- cool words haven't used yet
-- mechanism
-- symmetry
-- engine
-- semantics
-- agent
-- mind
-- mind
