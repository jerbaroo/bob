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
  , modelSkills :: [Skill]
  -- ^ Skills the model may pull in on demand (not user-activated).
  , modelSkillsActive :: [(Text, Text)]
  -- ^ User-activated skills as @(name, full SKILL.md content)@, injected
  --   directly into the prompt.
  , modelSoul :: Text
  , modelTemperature :: Temperature
  , modelTasks :: [Task]
  -- ^ Current tasks to complete.
  }
