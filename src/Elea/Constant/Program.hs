-- | ...
-- Module: Base
-- Description : Base Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Constant.Program (
    elea
  ) where


import           Data.Foldable (foldl')

import qualified Elea.Def as Def
import qualified Elea.Obj as Obj


elea :: Obj.Program
elea = Obj.initProgram program' initState Def.noIdentity
  where
    program' = Obj.Program' {
      identity = eleaIdentity
    , space    = eleaSpace  
    , time     = eleaTime  
    , agency   = eleaAgency
    }
    initState = Def.Reference {
        programId     = Def.ProgramId "elea"
      , abstractionId = Def.AbstractionId "program"
      , stateId       = Def.StateId "view-basic"
      , arrowId       = Nothing
    }


eleaIdentity :: Def.ProgramIdentity
eleaIdentity = Def.ProgramIdentity {
    id          = Def.ProgramId "elea"
  , name        = Def.ProgramName "Elea"
  , description = Def.ProgramDescription ""
}


-- | Space
--------------------------------------------------------------------------------

-- | Space
eleaSpace :: Obj.Space 
eleaSpace = Obj.newSpace abstractions
  where
    abstractions = [absProgram]

-- | Abstraction: Program
absProgram :: Obj.Abstraction
absProgram = 
  let stateView = Obj.State {
          id          = Def.StateId "view-basic"
        , name        = Just $ Obj.StateName "View"
        , description = Just $ Obj.StateDescription ""
        }
      states = [
          stateView
        ]
      initAbs = Obj.newAbstraction $ Def.AbstractionId "program"
      abs = foldl' Obj.abstractionWithState initAbs states
  in  abs {
          Obj.name        = Just $ Def.AbstractionName "Program"
        , Obj.description = Nothing
      }


-- | Time
--------------------------------------------------------------------------------

-- | Time
eleaTime :: Def.Time
eleaTime = Def.Time {
    types     = []
  , computers = []
}


-- | Agency
--------------------------------------------------------------------------------

-- | Agency
eleaAgency :: Obj.Agency
eleaAgency = Obj.Agency {
      agents = Obj.Group []
    , epic   = Obj.Epic Def.existence []
  }

