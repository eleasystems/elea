-- | ...
-- Module: Base
-- Description : Base Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Types.Computation (
    Computer (..)
  , Computation (..)
  , SideEffect (..), Condition (..)
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


import Elea.Types.Composition (Arrow, Abstraction, Reference)


-- | Computer
data Computer = Computer {
    condition  :: Condition    
  , affordance :: ArrowReference
  , sideEffect :: SideEffect    
} deriving (Eq, Generic, Show)


-- | Computation
data Computation =
    ComputationSideEffect SideEffect
  | ComputationCondition Condition
  deriving (Eq, Generic, Show)


-- | Side Effect
-- Monadic relationship 
newtype SideEffect = SideEffect {
  abstraction :: Abstraction
} deriving (Eq, Generic, Show)


-- | Condition
-- Examples:
--  * In blockchain, the derivation are the resources which must be executed 
--    in order to add a new block
newtype Condition = Condition {
  abstraction :: Abstraction
} deriving (Eq, Generic, Show)

