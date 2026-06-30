module Mah.Agents.State.AgentOutput where

import Mah.Agents.State.AgentResolution (AgentResolution)
import Mah.Agents.State.AgentTask (AgentTask)
import Mah.Agents.State.AgentThought (AgentThought)

-- | The final output from the agent.
data AgentOutput = AgentOutput
  { resolution :: AgentResolution
  -- | Updated tasks for the current agent.
  , tasks :: [AgentTask]
  -- | Intermediary thoughts.
  , thoughts :: [AgentThought]
  }
