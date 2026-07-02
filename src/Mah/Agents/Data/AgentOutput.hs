module Mah.Agents.Data.AgentOutput where

import Mah.Agents.Data.AgentResolution (AgentResolution)
import Mah.Agents.Data.AgentTask (AgentTask)
import Mah.Agents.Data.AgentThought (AgentThought)

-- | The final output from the agent.
data AgentOutput = AgentOutput
  { resolution :: AgentResolution
  -- | Updated tasks for the current agent.
  , tasks :: [AgentTask]
  -- | Intermediary thoughts.
  , thoughts :: [AgentThought]
  }
