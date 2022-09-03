-- | ...
-- Module: Program
-- Description : Computer servers
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Program (
    Server (..)
  , Kind (..)
  ) where


import            Data.Aeson (
    FromJSON (..), (.:)
  , ToJSON (..), (.=)
  )
import           Data.Aeson.Types (prependFailure, typeMismatch)
import qualified Data.Aeson as JSON (
    object, withObject
  , Value (String)
  )
import           Data.Text (Text)
import           GHC.Generics

import           Elea.Types.Composition (Abstraction)
import           Elea.Types.Consensus (Story)


-- | Program
data Program = Program {
    features :: Abstraction
  , proofs   :: [Story] 
} deriving (Eq, Generic, Show)


