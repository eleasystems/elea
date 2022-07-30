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

import           Elea.Base (Computer, Object)


--------------------------------------------------------------------------------
-- | Computer Index
--------------------------------------------------------------------------------

data ComputerIndex = ComputerIndex {
    computers         :: [Computer]
  , computersByObject :: HashMap Object [Computer]
}


fromComputers :: [Computer] -> ComputerIndex
fromComputers computers = ComputerIndex {
    computers         = computers
  , computersByObject = foldl' go HM.empty computers
  }
  where
    go hm computer = HM.insertWith (++) computer.forObject [computer] hm


computersForObject :: ComputerIndex -> Object -> [Computer]
computersForObject index obj = 
  HM.findWithDefault [] obj index.computersByObject
