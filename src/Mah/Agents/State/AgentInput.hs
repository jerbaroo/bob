module Mah.Agents.State.AgentInput where

import Data.Text (Text)
import Mah.Agents.State.AgentTask (AgentTask)
import Mah.Agents.State.Event (Event)

-- | Input available to an agent.
data AgentInput = AgentInput
  { events :: [Event]
  -- | Current tasks in progress.
  , tasks :: [AgentTask]
  , userInput :: Text
  }

emptyAgentInput :: Text -> AgentInput
emptyAgentInput userInput = AgentInput
  { events = []
  , tasks = []
  , userInput
  }
