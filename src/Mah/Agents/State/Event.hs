module Mah.Agents.State.Event where

import Data.Text (Text)
import Mah.Agents.State.AgentOutput (AgentOutput)
import Mah.Agents.State.AgentTool (AgentTool, AgentToolArgs)
import Mah.Agents.State.Command (Command)

-- | Something that has happened.
data Event
  = Event_AgentCall
  | Event_AgentOutput AgentOutput
  | Event_CommandRun Command
  -- | Event_AgentSpawned AgentId AgentId Task
  | Event_ToolCall AgentTool AgentToolArgs -- TODO include output
  -- | User_AgentCancel
  | Event_UserInput Text
