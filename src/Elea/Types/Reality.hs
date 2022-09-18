-- | ...
-- Module: Elea.Types.Reality
-- Description: Base Elea types
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
    PhenomenaComposition Abstraction
  | PhenomenaCasuality Server
  | PhenomenaConsensus Story
  deriving (Eq, Generic, Show)


-- | Proof
data Proof = Proof {
    composition :: Abstraction 
  , casuality   :: Engine 
  , consensus   :: Story 
} deriving (Eq, Generic, Show)


-- | Program
newtype Program = Program {
  proofs :: [Proof]
} deriving (Eq, Generic, Show)



-- | Consciousness
data Consciousness = 
    LocalConsciousness
  | SharedConsciousness

