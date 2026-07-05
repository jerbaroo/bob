module Agent.Bob.Data.HarnessState where

import Data.Map (Map)
import Data.Map qualified as Map
import Agent.Bob.Prelude hiding (read)
import Agent.Bob.Data.AgentId (AgentId)
import Agent.Bob.Data.Task (Task)
import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.Event (Event)
import Agent.Bob.Data.Role (Role)
import Agent.Bob.Data.Temperature (Temperature)

data HarnessState = HarnessState
  { events :: Map AgentId [Event]
  -- ^ A history of all that occurred.
  -- Everything else in this record should be derivable from the events.
  , loggingRole :: Maybe Role
  , tasks :: Map AgentId [Task]
  -- ^ The current tasks per-agent.
  , modelTemperature :: Temperature
  }

mkHarnessState :: Config -> HarnessState
mkHarnessState Config{..} = HarnessState
  { events = Map.empty
  , loggingRole = Nothing
  , modelTemperature
  , tasks = Map.empty
  }
