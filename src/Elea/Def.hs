-- | ...
-- Module: Elea.Types.Eleaa
-- Description: Core Elea types
--
-- Describe this module more here.
-- ...

{-# LANGUAGE DeriveGeneric #-}

module Elea.Def (
    -- Reality
    Phenomena (..)
  , ObjectType (..), objectTypeFromText, objectTypeAsText
  , ObjectRef (..), objectRefFromText
    -- Space
  , Space (..)
  , Abstraction (..)
  , AbstractionId (..), AbstractionName (..), AbstractionDescription (..)
  , StateId (..)
  , Arrow (..), ArrowId (..)
    -- Time
  , Time (..), noTime, absence
  , Mutation (..), Type (..)
  , Computer (..), ComputerId (..), magic
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
  , ProgramIdentity (..), noIdentity
  , ProgramId (..), ProgramName (..), ProgramDescription (..)
  , Reference (..), nowhere, void
  ) where


import           Data.Aeson (
    ToJSON (..), (.=)
  , FromJSON (..), (.:)
  )
import qualified Data.Aeson as JSON (
    Value (..)
  , object, withObject
  )
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           Data.ByteString (ByteString)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import qualified Data.Text as T (drop, pack, toLower)
import           GHC.Generics

--------------------------------------------------------------------------------
-- REALITY / EXISTENCE
--------------------------------------------------------------------------------

-- | Phenomena
-- Reality as a phenomena are its inherent phenomena
data Phenomena = 
    PhenomenaComposition
  | PhenomenaCasuality
  | PhenomenaConsensus
  deriving (Eq, Generic, Show)


-- | ObjectType
data ObjectType = 
    ObjectTypeAbstraction
  | ObjectTypeState
  | ObjectTypeArrow
  | ObjectTypeComputer
  | ObjectTypeType
  | ObjectTypeMutation
  | ObjectTypeEffect
  | ObjectTypeProperty
  | ObjectTypeAgent
  | ObjectTypeStory
  | ObjectTypeEvent
  deriving (Eq, Generic, Show)


objectTypeFromText :: Text -> Maybe ObjectType
objectTypeFromText "abstraction" = Just ObjectTypeAbstraction
objectTypeFromText "state"       = Just ObjectTypeState
objectTypeFromText "arrow"       = Just ObjectTypeArrow
objectTypeFromText "computer"    = Just ObjectTypeComputer
objectTypeFromText "type"        = Just ObjectTypeType
objectTypeFromText "mutation"    = Just ObjectTypeMutation
objectTypeFromText "effect"      = Just ObjectTypeEffect
objectTypeFromText "property"    = Just ObjectTypeProperty
objectTypeFromText "agent"       = Just ObjectTypeAgent
objectTypeFromText "story"       = Just ObjectTypeStory
objectTypeFromText "event"       = Just ObjectTypeEvent
objectTypeFromText _             = Nothing

objectTypeAsText :: ObjectType -> Text
objectTypeAsText ObjectTypeAbstraction = "abstraction"
objectTypeAsText ObjectTypeState       = "state"
objectTypeAsText ObjectTypeArrow       = "arrow"
objectTypeAsText ObjectTypeComputer    = "computer"
objectTypeAsText ObjectTypeType        = "type"
objectTypeAsText ObjectTypeMutation    = "mutation"
objectTypeAsText ObjectTypeEffect      = "effect"
objectTypeAsText ObjectTypeProperty    = "property"
objectTypeAsText ObjectTypeAgent       = "agent"
objectTypeAsText ObjectTypeStory       = "story"
objectTypeAsText ObjectTypeEvent       = "event"
    
-- | ObjectRef
data ObjectRef = 
    ObjectRefAbstraction AbstractionId
  | ObjectRefState StateId
  | ObjectRefArrow ArrowId
  | ObjectRefComputer ComputerId
  | ObjectRefType TypeId
  | ObjectRefProperty PropertyId
  | ObjectRefAgent AgentId
  | ObjectRefStory StoryId
  | ObjectRefEvent EventId
  deriving (Eq, Generic, Show)


objectRefFromText :: ObjectType -> Text -> ObjectRef
objectRefFromText objectType t = case objectType of
  ObjectTypeAbstraction -> ObjectRefAbstraction $ AbstractionId t
  ObjectTypeState       -> ObjectRefState $ StateId t
  ObjectTypeArrow       -> ObjectRefArrow $ ArrowId t
  ObjectTypeComputer    -> ObjectRefComputer $ ComputerId t
  ObjectTypeType        -> ObjectRefType $ TypeId t
  ObjectTypeProperty    -> ObjectRefProperty $ PropertyId t
  ObjectTypeAgent       -> ObjectRefAgent $ AgentId t
  ObjectTypeStory       -> ObjectRefStory $ StoryId t
  ObjectTypeEvent       -> ObjectRefEvent $ EventId t


--------------------------------------------------------------------------------
-- SPACE / COMPOSITION
--------------------------------------------------------------------------------

-- | Composition in form
data Space = Space {
    abstractions :: [Abstraction]
  , states       :: [StateId]
  , arrows       :: [Arrow]
} deriving (Eq, Generic, Show)


-- | Abstraction
data Abstraction = Abstraction {
    id     :: AbstractionId
  , states :: [StateId]
} deriving (Eq, Generic, Show)

instance ToJSON Abstraction

instance FromJSON Abstraction

-- | AbstractionId
newtype AbstractionId = AbstractionId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON AbstractionId where
  toJSON (AbstractionId s) = JSON.String s

instance FromJSON AbstractionId where
  parseJSON (JSON.String t) = return $ AbstractionId t
  parseJSON invalid         =
    prependFailure "parsing AbstractionId failed, "
        (typeMismatch "String" invalid)

instance Hashable AbstractionId

-- | Abstraction Name
newtype AbstractionName = AbstractionName {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON AbstractionName where
  toJSON (AbstractionName s) = JSON.String s

instance FromJSON AbstractionName where
  parseJSON (JSON.String t) = return $ AbstractionName t
  parseJSON invalid         =
    prependFailure "parsing AbstractionName failed, "
        (typeMismatch "String" invalid)


-- | Abstraction Description
newtype AbstractionDescription = AbstractionDescription {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON AbstractionDescription where
  toJSON (AbstractionDescription s) = JSON.String s

instance FromJSON AbstractionDescription where
  parseJSON (JSON.String t) = return $ AbstractionDescription t
  parseJSON invalid         =
    prependFailure "parsing AbstractionDescription failed, "
        (typeMismatch "String" invalid)


-- | StateId
newtype StateId = StateId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON StateId where
  toJSON (StateId s) = JSON.String s

instance FromJSON StateId where
  parseJSON (JSON.String t) = return $ StateId t
  parseJSON invalid         =
    prependFailure "parsing StateId failed, "
        (typeMismatch "String" invalid)

instance Hashable StateId


-- | Arrow
-- Talk about different types of arrows. Experience arrow
-- An arrow is just a relation
data Arrow = Arrow {
    id             :: ArrowId
  , initStateIdRef :: StateId
  , termStateIdRef :: StateId
} deriving (Eq, Generic, Show)

instance ToJSON Arrow where
  toJSON arrow = JSON.object [
      "id"             .= arrow.id 
    , "init_state_ref" .= arrow.initStateIdRef
    , "term_state_ref" .= arrow.termStateIdRef
    ]

instance FromJSON Arrow

-- | ArrowId
newtype ArrowId = ArrowId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON ArrowId where
  toJSON (ArrowId s) = JSON.String s

instance FromJSON ArrowId where
  parseJSON (JSON.String t) = return $ ArrowId t
  parseJSON invalid         =
    prependFailure "parsing ArrowId failed, "
        (typeMismatch "String" invalid)

instance Hashable ArrowId

--------------------------------------------------------------------------------
-- TIME / CAUSALITY
--------------------------------------------------------------------------------

-- | Causality in form
data Time = Time {
    types      :: [Type]
  , computers  :: [Computer]
  , effects    :: [Effects]
  , properties :: [Property]
  , mutations  :: [Mutation]
} deriving (Eq, Generic, Show)

-- | Time with no computers or types
noTime :: Time
noTime = Time [] [] [] [] []


-- | Mutation
data Mutation = 
    MutationAbstraction AbstractionMutation
  | MutationComposition CompositionMutation
  deriving (Eq, Generic, Show)


data AbstractionMutation =
    Addition Arrow
  | Subtraction Reference
  deriving (Eq, Generic, Show)


data CompositionMutation =
    CompositionAbstraction
  | CompositionProgram
  deriving (Eq, Generic, Show)


-- | Type
data Type = Type {
    id     :: Text
  , change :: [Mutation]
} deriving (Eq, Generic, Show)


void :: Type
void = Type {
    id     = "nochange"
  , change = []
}


-- | TypeId
-- type as set of mutations sticks to original idea from last summer
-- how would this interact with database ideas?
newtype TypeId = TypeId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON TypeId where
  toJSON (TypeId s) = JSON.String s

instance FromJSON TypeId where
  parseJSON (JSON.String t) = return $ TypeId t
  parseJSON invalid         =
    prependFailure "parsing TypeId failed, "
        (typeMismatch "String" invalid)


-- | Computer
-- cannot prove a computer
-- that's why stories only prove abstractions
-- stories cannot prove computers because of the halting problem
-- TODO can we just assume the abstraction?
-- note: computers are an interesting phenomological object because they 
-- implement change, but the implementation itself is a change that is 
-- never completely defined.
-- think: python function -> assembly -> logic gates -> voltages -> ? 
-- computers exist at different levels of abstraction but it's unclear
-- if there is a lowest level. quantum fields? but how are those implemented?
-- it's likely that there is some explanation, but that the idea of computation
-- is itself somewhat fabricated on top of the base layer and so it leads
-- us to weird conclusions (think: zero ontology kind of stuff). 
-- everything is just symmetries?  feels better, but still doesn't answer
-- question
data Computer = Computer {
    id         :: ComputerId
    -- Reference
  , reference  :: Reference
    -- Type
  , type_      :: Type 
    -- Effects
  , effects    :: Maybe Effects
     --properties are computers which represent what the
     --actual computer will do. they are like promises, 
     --promises that the computer will have a certain behavior
     --evidence?
  , properties :: Maybe [Property]
} deriving (Eq, Generic, Show)


-- | Magical computer
magic :: Reference -> Computer
magic ref = Computer {
    id         = ComputerId "magic"
  , reference  = ref
  , type_      = void
  , effects    = Nothing
  , properties = Nothing
}

newtype ComputerId = ComputerId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON ComputerId where
  toJSON (ComputerId s) = JSON.String s

instance FromJSON ComputerId where
  parseJSON (JSON.String t) = return $ ComputerId t
  parseJSON invalid         =
    prependFailure "parsing ComputerId failed, "
        (typeMismatch "String" invalid)

instance Hashable ComputerId


-- | Property
data Property = Property {
    id    :: PropertyId
  , story :: StoryId
} deriving (Eq, Generic, Show)


-- | PropertyId
-- type as set of mutations sticks to original idea from last summer
-- how would this interact with database ideas?
newtype PropertyId = PropertyId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON PropertyId where
  toJSON (PropertyId s) = JSON.String s

instance FromJSON PropertyId where
  parseJSON (JSON.String t) = return $ PropertyId t
  parseJSON invalid         =
    prependFailure "parsing PropertyId failed, "
        (typeMismatch "String" invalid)


-- | Effects
newtype Effects = Effects {
    storyIds :: [StoryId] 
} deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- AGENCY
--------------------------------------------------------------------------------

-- | Agency in form
data Agency = Agency {
    agents :: Group
  , epic   :: Epic
} deriving (Eq, Generic, Show)
-- | NoTime synonyms
never    = noTime
atNoTime = noTime
-- contradiction?
-- paradox?

-- | Default agency
absence :: Agency
absence = Agency (Group []) (Epic [] [])

forgotten = absence


-- | Group
newtype Group = Group {
    agents :: [Agent]
} deriving (Eq, Generic, Show)

-- | Epic
data Epic = Epic {
    past   :: [History]
  , future :: [Idea]
} deriving (Eq, Generic, Show)


-- | Event
data Event = Event {
    id       :: EventId
  , computer :: Computer
} deriving (Eq, Generic, Show)


-- | EventId
newtype EventId = EventId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON EventId where
  toJSON (EventId s) = JSON.String s

instance FromJSON EventId where
  parseJSON (JSON.String t) = return $ EventId t
  parseJSON invalid         =
    prependFailure "parsing EventId failed, "
        (typeMismatch "String" invalid)


-- | Agent
data Agent = Agent {
    id        :: AgentId
  , computers :: [Computer]
} deriving (Eq, Generic, Show)


god :: Agent 
god = Agent {
    id        = AgentId "god"
  , computers = []
}

-- | AgentId
newtype AgentId = AgentId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON AgentId where
  toJSON (AgentId s) = JSON.String s

instance FromJSON AgentId where
  parseJSON (JSON.String t) = return $ AgentId t
  parseJSON invalid         =
    prependFailure "parsing AgentId failed, "
        (typeMismatch "String" invalid)


-- | Story
--      
--                  Idea
--             |------------------
--  History    |          Proof
--  -----------| -----------------------
--             |
--             |     Idea 
--             | -----------
--
data Story = Story {
    id     :: StoryId
  , agent  :: Agent
  , events :: [Event]
} deriving (Eq, Generic, Show)


-- | Empty story
existence :: Story
existence = Story {
    id     = StoryId "existence"
  , agent  = god
  , events = []
}

-- | A story that has already been computed
type History = Story

-- | A story that may be computable
type Idea = Story

-- | A computable story/idea
type Proof = Story


-- | StoryId
newtype StoryId = StoryId {
  value :: Text
} deriving (Eq, Generic, Show)

instance ToJSON StoryId where
  toJSON (StoryId s) = JSON.String s

instance FromJSON StoryId where
  parseJSON (JSON.String t) = return $ StoryId t
  parseJSON invalid         =
    prependFailure "parsing StoryId failed, "
        (typeMismatch "String" invalid)

--------------------------------------------------------------------------------
-- PROGRAMMABILITY
--------------------------------------------------------------------------------

-- | Program
-- The base program definition. It is almost always within the compositional 
-- definition
data Program' = Program' {
    id      :: ProgramIdentity
  , space   :: Space
  , time    :: Time
  , agency  :: Agency
  , present :: Reference 
} deriving (Eq, Generic, Show)

-- | Program
-- Instead of composing programs by combining their components, we directly
-- combine the programs because it's useful to remember that a program is 
-- explicitly a combinatino of certain other programs
newtype Program = Program {
  programById :: HashMap ProgramId [Program']
} deriving (Eq, Generic, Show)


-- | Program identity
data ProgramIdentity = ProgramIdentity {
    id          :: ProgramId
  , name        :: ProgramName
  , description :: ProgramDescription
} deriving (Eq, Show)


-- | Vacuous program identity 
noIdentity :: ProgramIdentity 
noIdentity = ProgramIdentity {
    id          = ProgramId ""
  , name        = ProgramName ""
  , description = ProgramDescription ""
}


-- | Program Id
newtype ProgramId = ProgramId {
  value :: Text
} deriving (Eq, Generic, Show)


instance ToJSON ProgramId where
  toJSON (ProgramId s) = JSON.String s

instance FromJSON ProgramId where
  parseJSON (JSON.String t) = return $ ProgramId t
  parseJSON invalid         =
    prependFailure "parsing ProgramId failed, "
        (typeMismatch "String" invalid)

instance Hashable ProgramId


-- | Program Name
newtype ProgramName = ProgramName {
  value :: Text
} deriving (Eq, Show)


-- | Program Name
newtype ProgramDescription = ProgramDescription {
  value :: Text
} deriving (Eq, Show)


-- | Reference
data Reference = Reference {
    programId     :: ProgramId
  , abstractionId :: AbstractionId
  , stateId       :: StateId
  , arrowId       :: Maybe ArrowId
} deriving (Eq, Generic, Show)

instance ToJSON Reference

instance FromJSON Reference

-- | Empty reference
nowhere :: Reference
nowhere = Reference {
    programId     = ProgramId ""
  , abstractionId = AbstractionId ""
  , stateId       = StateId ""
  , arrowId       = Nothing
  }


--------------------------------------------------------------------------------
-- USABILITY
-- So far we've defined a lot of intersting concepts, but nothing that is 
-- actually usuable. We need to link the abstraction/conceptual to the reality
-- that we experience. This leads to explanations... ?
--------------------------------------------------------------------------------

-- should this be a monad? somehow seems correct
--   maybe need to think about what a force does
--   is kind of like an execute / things happen monad like IO
--
-- forces represent causality independently. in Elea of course a function/force
-- doesn't simply return a value, the value has to go somewhere. it goes into 
-- the creation of other abstractions which could be computed over by other 
-- agents. but for a force to be useful we still need to calculate its result
-- before we put it in its place
-- is a force = explanation?  is it ironic that I'm defining this to be an 
-- explanation but I'm having trouble finding an explanation for why it's
-- the right type?
-- so a Force is actually defined indp of the other Elea objects
class Force f d where  
  execute :: f -> [(d,d)] -> IO d


-- | Service
-- how to organize a service by the kind of computer it generates
-- automatically index properties? 
-- service implies the computablity of the computers it manages. it is what 
-- enables the computers to be useful. the "how" of this is never defined in a 
-- satisfactory way
-- not just defining how to access the computers, but actually how to call/use
-- them. this is where the "magic" happens. it is really magic because our 
-- explanations for how a force works will ultimately leave us unsatisfied
-- force is an analogy to physical forces e.g. gravity, weak/strong, electromagnetic
data Service f = Service {
    -- | The computer computer is computed with a callable with connections
    -- computation to the "real world" where we need to call a particular 
    -- computer on some device which needs to be located/accessed. The 
    -- locating/access/connecting-to of the computer is the computation for the
    -- computer computer.
    force     :: f
    -- | An interface for a computer computer is therefore the list of 
    -- computers than can be computed by accessing the service. Is also 
    -- synonymous with an API (application programming interface)
  , interface :: [Computer]
} deriving (Eq, Generic, Show)

type Server = Service
type Client = Service
type Index  = Service


-- | An Application is a usable program where all of the computers required 
-- by the program are implemented in some service
-- for example, the Elea UI is an application with develop + elea as programs
-- and could actually be implemented with any sort of service with relevant
-- forces. need to compose forces though
data Application f = Application {
    program :: Program
  , service :: Service f
} deriving (Eq, Generic, Show)

-- to figure out
-- how to use service to implement elea program computer
-- ui
-- elea ui is really composition of some services to make function
--   make service is the browser, which computes the application
--   the application is just a program derived from the main program
--   the application program is computed by browser computers but other 
--   parts need other computers available via services
--   the browser is a service since it provides http/html/css/js computers
--
-- need to index serives
-- find the program service 
-- genealize both connector/callable and lookup mechanism
--
-- backend
