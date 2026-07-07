module Agent.Bob.Data.HarnessState where

import Data.Map (Map)
import Data.Map qualified as Map
import Agent.Bob.Prelude hiding (read)
import Agent.Bob.Data.AgentId (AgentId)
import Agent.Bob.Data.Task (Task)
import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.Event (Event)
import Agent.Bob.Data.Role (Role)
import Agent.Bob.Data.Skill (Skill)
import Agent.Bob.Data.Temperature (Temperature)

data HarnessState = HarnessState
  { events :: Map AgentId [Event]
  -- ^ A history of all that occurred.
  -- Everything else in this record should be derivable from the events.
  , loggingRole :: Maybe Role
  -- ^ The currently active logger (i.e. who logged last).
  , tasks :: Map AgentId [Task]
  -- ^ The current tasks per-agent.
  , modelTemperature :: Temperature
  , sessionPath :: Text
  -- ^ The file this session's history is appended to. State, not Config: the
  -- user can rename (@/name@) or switch (@/resume@) it at runtime.
  , skills :: [Skill]
  -- ^ Skills as of the last scan. Rescanned at startup and before each model
  -- call (so newly added skills appear without a restart), then injected into
  -- the prompt.
  , soulText :: Maybe Text
  -- ^ The soul as of the last read; 'Nothing' until first read. Kept so a
  -- change to the soul file mid-session can be detected and reported.
  , userActivatedSkills :: [Text]
  -- ^ Names of skills the user activated (e.g. via @/caveman@). Their full
  -- content is included in every prompt; the rest stay opt-in for the model.
  }

mkHarnessState :: Config -> Text -> HarnessState
mkHarnessState Config{..} sessionPath = HarnessState
  { events = Map.empty
  , loggingRole = Nothing
  , modelTemperature
  , sessionPath
  , skills = []
  , soulText = Nothing
  , tasks = Map.empty
  , userActivatedSkills = []
  }
