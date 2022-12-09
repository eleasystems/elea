module Elea ( 
    -- Reality
    Phenomena (..)
    -- Space
  , Space (..), newSpace
  , Abstraction (..), newAbstraction, abstractionWithState
  , AbstractionId (..)
  , State (..), StateId (..), StateName (..), StateDescription (..)
  , Arrow (..), ArrowId (..)
  , Reference (..), nowhere
    -- Time
  , Time (..), noTime, absence
  , Mutation (..), Type (..)
  , Computer (..), magic
  , Effects (..), Property (..)
    -- Agency
  , Agency (..)
  , Group (..), Epic (..)
  , Story (..), History, Idea, Proof
  , existence
  , StoryId (..)
  , Event (..), EventId (..)
  , Agent (..), AgentId (..)
    -- Programs
  , Program (..)
  , programName, programAbstractionName, programStateName
  , currentProgramName
  , Program' (..)
  , initProgram
  , ProgramIdentity (..), noIdentity
  , ProgramId (..), ProgramName (..), ProgramDescription (..)
  , currentState, presentState
  , programHistory, programHistoryStates, currentProgramHistoryStates
  , next
  ) where


import Elea.Def (
    Phenomena (..)
  , AbstractionId (..)
  , StateId (..)
  , Arrow (..), ArrowId (..)
  , Reference (..), nowhere
  , Time (..), noTime, absence
  , Mutation (..), Type (..)
  , Computer (..), magic
  , Effects (..), Property (..)
  , Story (..), History, Idea, Proof
  , existence
  , StoryId (..)
  , Event (..), EventId (..)
  , Agent (..), AgentId (..)
  , ProgramIdentity (..), noIdentity
  , ProgramId (..), ProgramName (..), ProgramDescription (..)
  )
import Elea.Obj (
    Space (..), newSpace
  , Abstraction (..), newAbstraction, abstractionWithState
  , State (..), StateName (..), StateDescription (..)
  , Agency (..)
  , Group (..), Epic (..)
  , Program (..)
  , programName, programAbstractionName, programStateName
  , currentProgramName
  , Program' (..)
  , initProgram
  , currentState, presentState
  , programHistory, programHistoryStates, currentProgramHistoryStates
  , next
  )
