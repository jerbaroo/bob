module Mah.Agents.Data.HarnessState where

import Data.Map (Map)
import Data.Map qualified as Map
import Prelude hiding (read)
import Mah.Agents.Data.AgentId (AgentId)
import Mah.Agents.Data.AgentTask (AgentTask)
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.Event (Event)
import Mah.Agents.Data.Temperature (Temperature)

data HarnessState = HarnessState
  { events :: Map AgentId [Event]
  -- ^ A history of all that occurred.
  -- Everything else in this record should be derivable from the events.
  , tasks :: Map AgentId [AgentTask]
  -- ^ The current tasks per-agent.
  , temperature :: Temperature
  }

mkHarnessState :: Config -> HarnessState
mkHarnessState Config{..} = HarnessState
  { events = Map.empty
  , tasks = Map.empty
  , temperature
  }
