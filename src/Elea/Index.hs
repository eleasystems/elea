-- | ...
-- Module: Index
-- Description : Elea indexes
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Index (
    ComputerIndex, fromComputers
  , computersForObject
  ) where


import           Data.Aeson (
    FromJSON (..)
  , (.:), withObject
  )
import qualified Data.Aeson as JSON (Value (..))
import           Data.Foldable (foldl')
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM (
    empty
  , insertWith
  , findWithDefault
  )
import           Data.Text (Text)
import           GHC.Generics

import           Elea.Base (Computer, Def)


--------------------------------------------------------------------------------
-- | Computer Index
--------------------------------------------------------------------------------

data ComputerIndex = ComputerIndex {
    computers         :: [Computer]
  , computersByObject :: HashMap Def [Computer]
}

instance FromJSON ComputerIndex where
  parseJSON = withObject "ComputerIndex" $ \v -> fromComputers
    <$> v .: "computers"



fromComputers :: [Computer] -> ComputerIndex
fromComputers computers = ComputerIndex {
    computers         = []
  , computersByObject = foldl' go HM.empty computers
  }
  where
    go hm computer = HM.insertWith (++) computer.forDef [computer] hm


computersForObject :: ComputerIndex -> Def -> [Computer]
computersForObject index def = 
  HM.findWithDefault [] def index.computersByObject


