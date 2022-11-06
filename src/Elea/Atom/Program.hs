-- | ...
-- Module: Program
-- Description : Computer servers
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Atom.Program (
    Space (..)
  , Time (..)
  , Agency (..)
  , Program (..)
  , Destiny (..)
  ) where


import           GHC.Generics

import Elea.Atom.Phenomena


-- | Composition in form
data Space = Space {
    abstractions :: [Abstraction]
  , states       :: [State]
  , arrows       :: [Arrow]
} deriving (Eq, Generic, Show)


-- | Causality in form
data Time = Time {
    types     :: [Abstraction]
  , computers :: [State]
} deriving (Eq, Generic, Show)


-- | Agency in form
data Agency = Agency {
    agents  :: [Agent]
  , stories :: [Story]
} deriving (Eq, Generic, Show)


-- | Program
data Program = Program {
    space  :: Space
  , time   :: Time
  , agency :: Agency
} deriving (Eq, Generic, Show)


-- | A Destiny is a composition of programs
data Destiny = Destiny
