module Mah.Agents.Data.AgentInput where

import Data.Text (Text)
import Mah.Agents.Data.Task (Task)
import Mah.Agents.Data.Event (Event)
import Mah.Agents.Data.Temperature (Temperature)
import Mah.Agents.Data.ToolResult (ToolResult)
import Mah.Agents.Data.UserInput (UserInput)

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
