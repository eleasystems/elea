-- | ...
-- Module: Program
-- Description : Computer servers
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Object.Program (
    Space (..)
  , Time (..)
  , Agency (..)
  , Program (..)
  , Destiny (..)
  ) where


import Data.Text (Text)
import GHC.Generics

import Elea.Object.Phenomena as Object

  
-- | Composition in form
data Space = Space {
    abstractions :: [Object.Abstraction]
  , domains      :: [Text]
} deriving (Eq, Generic, Show)


-- | Causality in form
data Time = Time {
    types     :: [Object.Abstraction]
  , computers :: [Object.State]
} deriving (Eq, Generic, Show)


-- | Agency in form
data Agency = Agency {
    agents  :: [Object.Agent]
  , stories :: [Object.Story]
  , events  :: [Object.Event]
} deriving (Eq, Generic, Show)

-- | Program
data Program = Program {
    space  :: Space
  , time   :: Time
  , agency :: Agency
} deriving (Eq, Generic, Show)


-- | A Destiny is a composition of programs
data Destiny = Destiny
