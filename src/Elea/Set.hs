-- | ...
-- Module: Set
-- Description : Elea sets
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Set (
    SetKind (..)
  , Set (..)
  ) where


import           Data.Text (Text)
import           GHC.Generics


--------------------------------------------------------------------------------
-- | Set
--------------------------------------------------------------------------------

data SetKind =
    Servers 
  | Stories
  | Computers
  deriving (Eq, Show)


-- | Types of sets in Elea
data Set = 
    SetServer ServerSet
  | SetAbsStates AbsStatesSet
  deriving (Eq, Generic, Show)


-- | Define collections of servers
data ServerSet = ServerSet
  deriving (Eq, Generic, Show)


-- | Define collections of states in an abstraction
data AbsStatesSet = AbsStatesSet
  deriving (Eq, Generic, Show)
