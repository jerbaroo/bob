module Mah.Agents.Data.AgentInput where

import Data.Text (Text)
import Mah.Agents.Data.AgentTask (AgentTask)
import Mah.Agents.Data.Event (Event)

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
