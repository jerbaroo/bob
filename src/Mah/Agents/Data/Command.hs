module Mah.Agents.Data.Command where

import Data.Text (Text)
import Mah.Agents.Pretty (Pretty(..))
import Mah.Agents.Data.AgentTool (AgentTool, AgentToolArgs)
import Mah.Agents.Data.Event (IsEvent(..))
import Mah.Agents.Data.UserInput (UserInput)
import Witch (from)

-- | A side-effect the harness needs to perform.
-- TODO make this a type class
data Command
  = CallAgent UserInput
  | RunTool AgentTool AgentToolArgs
  | ReadConsole
  | WriteConsole Text
  deriving Show

instance IsEvent Command

instance Pretty Command where
  pretty (CallAgent userInput) = "Calling agent with user input: " <> from userInput
  pretty (RunTool tool args) = "Run tool " <> from (show tool) <> from (show args)
  pretty ReadConsole = "Read from console"
  pretty (WriteConsole text) = "Write to console: " <> text
