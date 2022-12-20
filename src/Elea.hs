module Elea ( 
    -- Reality
    Phenomena (..)
  , Object (..)
  , ObjectType (..), objectTypeFromText, objectTypeAsText
  , ObjectRef (..), objectRefFromText
    -- Space
  , Space (..), newSpace
  , spaceStates, spaceArrows
  , Abstraction (..), newAbstraction
  , abstractionWithState, abstractionWithArrow
  , AbstractionId (..), AbstractionName (..), AbstractionDescription (..)
  , State (..), StateId (..), StateName (..), StateDescription (..)
  , Arrow (..), ArrowId (..), ArrowName (..), ArrowDescription (..)
  , Reference (..), nowhere
    -- Time
  , Time (..), newTime
  , noTime, absence
  , timeWithComputer
  , Mutation (..), Type (..)
  , Computer (..), newComputer, magic
  , ComputerId (..), ComputerName (..), ComputerDescription (..)
  , Effects (..), Property (..)
    -- Agency
  , Agency (..)
  , AgentId (..)
  , Agent (..), AgentName (..), AgentDescription (..)
  , Epic (..)
  , Story (..), History, Idea, Proof
  , existence
  , StoryId (..)
  , Event (..), EventId (..)
    -- Programs
  , Program (..)
  , programName, programAbstractionName, programStateName
  , currentProgram, currentProgramName
  , Program' (..), object
  , initProgram
  , ProgramIdentity (..), noIdentity
  , ProgramId (..), ProgramName (..), ProgramDescription (..)
  , currentState, presentState
  , programHistory, programHistoryStates, currentProgramHistoryStates
  , next
  ) where


import Elea.Def (
    Phenomena (..)
  , ObjectType (..), objectTypeFromText, objectTypeAsText
  , ObjectRef (..), objectRefFromText
  , AbstractionId (..), AbstractionName (..), AbstractionDescription (..)
  , ComputerId (..)
  , StateId (..)
  , ArrowId (..)
  , Reference (..), nowhere
  , noTime, absence
  , Mutation (..), Type (..)
  , magic
  , Effects (..), Property (..)
  , Story (..), History, Idea, Proof
  , existence
  , StoryId (..)
  , Event (..), EventId (..)
  , AgentId (..)
  , ProgramIdentity (..), noIdentity
  , ProgramId (..), ProgramName (..), ProgramDescription (..)
  )
import Elea.Obj (
    Object (..)
  , Space (..), newSpace
  , spaceStates, spaceArrows
  , Abstraction (..), newAbstraction
  , abstractionWithState, abstractionWithArrow
  , State (..), StateName (..), StateDescription (..)
  , Arrow (..), ArrowName (..), ArrowDescription (..)
  , Time (..), newTime
  , timeWithComputer
  , Computer (..), newComputer
  , ComputerName (..), ComputerDescription (..)
  , Agency (..)
  , Agent (..), AgentName (..), AgentDescription (..)
  , Epic (..)
  , Program (..)
  , programName, programAbstractionName, programStateName
  , currentProgram, currentProgramName
  , Program' (..)
  , initProgram
  , object
  , currentState, presentState
  , programHistory, programHistoryStates, currentProgramHistoryStates
  , next
  )
