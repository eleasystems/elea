-- | ...
-- Module: Elea.Types.Eleaa
-- Description: Core Elea types
--
-- Stateful versions of Elea definitions
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Obj (
    -- Space
    Space (..), newSpace, emptySpace
  , Abstraction (..)
  , newAbstraction, abstractionWithState
  , State (..)
  , StateName (..), StateDescription (..)
    -- Time
    -- Agency
  , Agency (..)
  , Group (..), Epic (..)
    -- Programs
  , Program (..)
    -- Program reference data
  , programName, programAbstractionName, programStateName
  , currentProgramName
  , Program' (..)
  , initProgram
  , currentState, presentState
  , programHistory, programHistoryStates, currentProgramHistoryStates
  , next
  ) where


import           Data.Aeson (
    ToJSON (..), (.=)
  , FromJSON (..), (.:)
  , genericToEncoding, defaultOptions
  )
import qualified Data.Aeson as JSON (
    Value (..)
  , object, withObject
  )
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           Data.Foldable (foldl')
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM (
    empty, singleton
  , insert
  , lookup
  )
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T (drop, pack, toLower)
import           GHC.Generics

import Elea.Def (computer, reference, events)
import qualified Elea.Def as Def


import Debug.Trace (traceShow)


--------------------------------------------------------------------------------
-- SPACE / COMPOSITION
--------------------------------------------------------------------------------

-- | Composition in form
data Space = Space {
    abstractions :: [Abstraction]
  , domains      :: [Text]
  , absById      :: HashMap Def.AbstractionId Abstraction
} deriving (Eq, Generic, Show)

-- | Constructor
newSpace :: [Abstraction] -> Space
newSpace _abstractions = Space {
    abstractions = _abstractions
  , domains      = []
  , absById      = foldl' go HM.empty _abstractions
  }
  where
    go hm abs = HM.insert abs.id abs hm


-- | Space with no abstraction
emptySpace :: Space
emptySpace = newSpace []

-- | Synonyms
emptiness    = emptySpace
spaciousness = emptySpace
nothing      = emptySpace
voidness     = emptySpace

-- | Get abstraction by id
abstractionWithId :: Space -> Def.AbstractionId -> Maybe Abstraction
abstractionWithId space absId = HM.lookup absId space.absById


-- | Abstraction
data Abstraction = Abstraction {
    id          :: Def.AbstractionId
  , name        :: Maybe Def.AbstractionName
  , description :: Maybe Def.AbstractionDescription
  , states      :: [State]
  , stateById   :: HashMap Def.StateId State
} deriving (Eq, Generic, Show)


-- | Constructor
newAbstraction :: Def.AbstractionId -> Abstraction
newAbstraction absId = Abstraction {
    id          = absId
  , name        = Nothing
  , description = Nothing
  , states      = []
  , stateById   = HM.empty
  }
  --, stateById   = foldl' go HM.empty states
  --}
  --where
    --go hm state = HM.insert state.id state hm


stateWithId :: Abstraction -> Def.StateId -> Maybe State
stateWithId abs stateId = HM.lookup stateId abs.stateById

abstractionWithState :: Abstraction -> State -> Abstraction
abstractionWithState abs state = abs {
     -- TODO remove/prevent duplicate / use set
    states      = state : abs.states
  , stateById   = HM.insert state.id state abs.stateById
  }


-- | State
data State = State {
    id          :: Def.StateId
  , name        :: Maybe StateName
  , description :: Maybe StateDescription
} deriving (Eq, Generic, Show)

instance ToJSON State where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON State

-- | State Name
newtype StateName = StateName {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON StateName where
  toJSON (StateName s) = JSON.String s

instance FromJSON StateName where
  parseJSON (JSON.String t) = return $ StateName t
  parseJSON invalid         =
    prependFailure "parsing StateName failed, "
        (typeMismatch "String" invalid)


-- | StateDescription
newtype StateDescription = StateDescription {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON StateDescription where
  toJSON (StateDescription s) = JSON.String s

instance FromJSON StateDescription where
  parseJSON (JSON.String t) = return $ StateDescription t
  parseJSON invalid         =
    prependFailure "parsing StateDescription failed, "
        (typeMismatch "String" invalid)

--------------------------------------------------------------------------------
-- TIME / CAUSALITY
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- AGENCY
--------------------------------------------------------------------------------

-- | Agency in form
data Agency = Agency {
    agents :: Group
  , epic   :: Epic
} deriving (Eq, Generic, Show)

-- | Default agency
absence :: Agency
absence = Agency (Group []) (Epic Def.existence [])


-- | Group
newtype Group = Group {
    agents :: [Def.Agent]
} deriving (Eq, Generic, Show)


-- | Epic
data Epic = Epic {
    past   :: Def.History
  , future :: [Def.Idea]
} deriving (Eq, Generic, Show)



--------------------------------------------------------------------------------
-- PROGRAMMABILITY
--------------------------------------------------------------------------------

---- | Causality in form
--data Time = Time {
    --types     :: [Abstraction]
  --, computers :: [Object.State]
--} deriving (Eq, Generic, Show)



-- | Program
-- A program contains both the future and past since they are one 
-- and the same part of the same duality.
data Program' = Program' {
    identity :: Def.ProgramIdentity
  , space    :: Space
  , time     :: Def.Time
  , agency   :: Agency
} deriving (Eq, Generic, Show)

-- | `present` synonym since it's sometimes used better as a noun
-- vs an adjective
current = present

data Program = Program {
    programById :: HashMap Def.ProgramId Program'
  , present     :: Def.Reference 
  , identity    :: Def.ProgramIdentity
} deriving (Eq, Generic, Show)




initProgram :: Program' -> Def.Reference -> Def.ProgramIdentity -> Program
initProgram _program' _present _identity = 
  let initalizedProgram' = next' _program' $ Def.magic _present
  in  Program {
    programById = HM.singleton (initalizedProgram'.identity.id) initalizedProgram'
  , present     = _present
  , identity    = _identity
  }


programWithId :: Program -> Def.ProgramId -> Maybe Program'
programWithId prog progId = HM.lookup progId prog.programById


-- | Get the present/current state of the Program
programState :: Program -> Def.Reference -> Maybe State
programState prog ref = do
  prog' <- programWithId prog ref.programId
  abs <- abstractionWithId prog'.space ref.abstractionId
  stateWithId abs ref.stateId


programName :: Program -> Def.Reference -> Maybe Text
programName program ref = do
  program' <- programWithId program ref.programId
  return program'.identity.name.value

programAbstractionName :: Program -> Def.Reference -> Maybe Text
programAbstractionName program ref = do
  program' <- programWithId program ref.programId
  abs <- abstractionWithId program'.space ref.abstractionId
  name <- abs.name
  return name.value

-- | 
programStateName :: Program -> Def.Reference -> Maybe Text
programStateName program ref = do
  program' <- programWithId program ref.programId
  abs <- abstractionWithId program'.space ref.abstractionId
  st <- stateWithId abs ref.stateId
  name <- st.name
  return name.value


programHistory :: Program -> Def.ProgramId -> Def.Story
programHistory program programId = 
  let program' = fromJust $ programWithId program programId
  in  program'.agency.epic.past


programHistoryStates :: Program -> Def.ProgramId -> [Def.Reference]
programHistoryStates program programId = 
  let history = programHistory program programId
  in  (\ev -> ev.computer.reference) <$> history.events


currentProgram :: Program -> Program'
currentProgram program = fromJust $ 
  programWithId program program.present.programId


currentProgramName :: Program -> Text
currentProgramName program = (currentProgram program).identity.name.value


currentProgramHistoryStates :: Program -> [Def.Reference]
currentProgramHistoryStates program = 
  programHistoryStates program program.present.programId


-- | Get the present state of a program
presentState :: Program -> State
presentState program =
  fromJust $ programState program program.present


-- | Try to transition a program to another state using the given computer
next' :: Program' -> Def.Computer -> Program'
next' program' _computer = 
  let newEvent    = Def.Event {
          id       = Def.EventId ""
        , computer = _computer
      }
      -- need to relearn lenses lol
  in  program' {
        agency = program'.agency {
          epic = program'.agency.epic {
            past = program'.agency.epic.past {
              events = newEvent : program'.agency.epic.past.events
            }
          }
        }
      }
    

-- Add in eithert type, just skipping maybe for now
-- could also try to prove that these maybes could never happen
--   with a valid implementation
next :: Program -> Def.Computer -> Program
next program _computer = 
  let program' = currentProgram program
      newProgram' = next' program' _computer
      programMap = HM.insert _computer.reference.programId newProgram' program.programById 
  in  Program {
          programById = programMap
        , present     = _computer.reference
      }

currentState = presentState


-- | The most trivial program with the most trivial identity
void :: Text -> Program
void programId = Program {
    programById = HM.singleton (program'.identity.id) program'
  , present     = Def.nowhere
  } 
  where
    program' = Program' {
        identity = Def.ProgramIdentity {
            id          = Def.ProgramId ""   
          , name        = Def.ProgramName ""   
          , description = Def.ProgramDescription ""   
        }
      , space    = emptySpace 
      , time     = Def.noTime 
      , agency   = absence 
    }

-- | Void synonyms
impossiblity = void
impossible   = void
paradoxical  = void


