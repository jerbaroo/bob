module Mah.Agents.State.HarnessState where

import Data.Map (Map)
import Data.Map qualified as Map
import Prelude hiding (read)
import Mah.Agents.State.AgentId (AgentId)
import Mah.Agents.State.AgentTask (AgentTask)
import Mah.Agents.State.Event (Event)

data HarnessState = HarnessState
  { events :: Map AgentId [Event]
  -- ^ A history of all that occurred.
  -- Everything else in this record should be derivable from the events.
  , tasks :: Map AgentId [AgentTask]
  -- ^ The current tasks per-agent.
  }

emptyHarnessState :: HarnessState
emptyHarnessState = HarnessState
  { events = Map.empty
  , tasks = Map.empty
  }
