module Agent.Bob.Data.AgentInput where

import Agent.Bob.Data.Task (Task)
import Agent.Bob.Data.Event (Event)
import Agent.Bob.Data.Skill (Skill)
import Agent.Bob.Data.Temperature (Temperature)
import Agent.Bob.Prelude

-- | Input available to an agent.
data AgentInput = AgentInput
  { events :: [Event]
  -- ^ History of events, including user input and tool results.
  --   May be a compressed version of the full history.
  , modelName :: Text
  , modelTemperature :: Temperature
  , modelSkills :: [Skill]
  -- ^ Skills available to pull in on demand.
  , modelSoul :: Text
  , modelTasks :: [Task]
  -- ^ Current tasks to complete.
  }
