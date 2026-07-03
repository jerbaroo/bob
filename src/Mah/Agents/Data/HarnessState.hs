module Mah.Agents.Data.HarnessState where

import Data.Map (Map)
import Data.Map qualified as Map
import Prelude hiding (read)
import Mah.Agents.Data.AgentId (AgentId)
import Mah.Agents.Data.Task (Task)
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.Event (Event)
import Mah.Agents.Data.Role (Role)
import Mah.Agents.Data.Temperature (Temperature)

data HarnessState = HarnessState
  { events :: Map AgentId [Event]
  -- ^ A history of all that occurred.
  -- Everything else in this record should be derivable from the events.
  , loggingRole :: Maybe Role
  , tasks :: Map AgentId [Task]
  -- ^ The current tasks per-agent.
  , temperature :: Temperature
  }

mkHarnessState :: Config -> HarnessState
mkHarnessState Config{..} = HarnessState
  { events = Map.empty
  , loggingRole = Nothing
  , tasks = Map.empty
  , temperature
  }
