-- | ...
-- Module: Base
-- Description : Base Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Constants (
    time
  ) where


import Elea.Types.Structure (
    Abstraction (..)
  , State (..), StateId (..), StateType (..)
  , Arrow (..)
  )


constant :: StateId -> StateId -> Arrow
constant initStateId termStateId = Arrow {
    initStateId = initStateId
  , termStateId = termStateId
  , mutations   = []
}



-- | Infinity
infinity :: StateId -> Abstraction
infinity stateId = 
  let arrowOfTime = constant stateId stateId
      possibilty = State {
        id     = stateId
      , arrows = [arrowOfTime]
      , type_  = InitialTerminal
    }
  in  Abstraction [possibilty]

-- | Space
space :: Abstraction
space = infinity $ StateId "everything"

-- | Time
-- A measurement of difficulty or separateness
-- without time would only be impermanence
time :: Abstraction
time = infinity $ StateId "forever"

-- | SpaceTime
-- Everything forever 
spacetime :: Abstraction
spacetime = time <> space 


-- code should speak for itself

-- set, plurality
-- computing set is an ordering
-- control flow

-- philosophy
-- ontology
-- epistemology

-- information
-- state
-- mutability / immutability
-- language
-- purity / referential transparnecy

-- determinism
-- probabilty
-- discrete / infinite

-- mathematics - domain of all languages
-- theory

-- truth

-- model
-- power
-- intelligence
-- dream

-- cooperation
-- consensus

-- curves, cycles

-- observer
-- subject / object

-- civilization
-- individual

-- meaning / signifance
-- pain / pleasure
-- agent
-- abstract / concrete / concept
