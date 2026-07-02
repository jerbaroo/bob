module Mah.Agents.Data.Command where

import Data.Text (Text)
import Mah.Agents.Pretty (Pretty(..))
import Mah.Agents.Data.AgentId (AgentId)
import Mah.Agents.Data.AgentTool (AgentTool, AgentToolArgs)
import Witch (from)

-- | A side-effect the harness needs to perform.
data Command
  = CallAgent AgentId
  | RunTool AgentTool AgentToolArgs
  | ReadConsole
  | WriteConsole Text
  deriving Show

instance Pretty Command where
  pretty (CallAgent agentId) = "Call " <> pretty agentId
  pretty (RunTool tool args) = "Run tool " <> from (show tool) <> from (show args)
  pretty ReadConsole = "Read from console"
  pretty (WriteConsole text) = "Write to console: " <> text
