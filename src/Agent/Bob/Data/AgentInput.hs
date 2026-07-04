module Agent.Bob.Data.AgentInput where

import Data.Text (Text)
import Agent.Bob.Data.Task (Task)
import Agent.Bob.Data.Event (Event)
import Agent.Bob.Data.Temperature (Temperature)
import Agent.Bob.Data.ToolResult (ToolResult)
import Agent.Bob.Data.UserInput (UserInput)

-- | Input available to an agent.
data AgentInput = AgentInput
  { events :: [Event]
  -- ^ History of events.
  --   May be a compressed version of the full history.
  , modelName :: Text
  , modelTemperature :: Temperature
  , modelSoul :: Maybe Text
  , modelTasks :: [Task]
  -- ^ Current tasks to complete.
  , userInput :: UserInput
  , toolResults :: [ToolResult]
  -- ^ Results of tools the model requested on the previous call, if any.
  --   The 'userInput' is kept unchanged while these accumulate.
  }
