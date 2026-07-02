module Mah.Agents.Data.Event where

import Data.Text (Text)
import Mah.Agents.Data.AgentOutput (AgentOutput)
import Mah.Agents.Data.AgentTool (AgentTool, AgentToolArgs)
import Mah.Agents.Data.Command (Command)

-- | Something that has happened.
data Event
  = Event_AgentCall
  | Event_AgentOutput AgentOutput
  | Event_CommandRun Command
  -- | Event_AgentSpawned AgentId AgentId Task
  | Event_ToolCall AgentTool AgentToolArgs -- TODO include output
  -- | User_AgentCancel
  | Event_UserInput Text
