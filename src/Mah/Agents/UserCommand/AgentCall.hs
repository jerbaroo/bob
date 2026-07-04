module Mah.Agents.UserCommand.AgentCall where

import Control.Monad (forM)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Mah.Agents.Command (IsCommand(..), recordEvents, step)
import Mah.Agents.Command.CallAgent qualified as CallAgent
import Mah.Agents.Data.AgentOutput (AgentOutput(..))
import Mah.Agents.Data.Event (IsEvent(..))
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Data.ToolResult (ToolResult(..))
import Mah.Agents.Data.UserInput (UserInput)
import Mah.Agents.Effect.Console (writeSection)
import Mah.Agents.Permissions (checkPermission)
import Mah.Agents.Tool (runToolForModel, toolName')
import Mah.Agents.UserCommand (IsUserCommand(..))
import Witch (from, into)

-- | Call the agent with the user's input and run any tools it requests. This
-- is the fallback user command: it parses any input, so in the registry it
-- must be tried last.
newtype AgentCall = AgentCall UserInput

instance IsUserCommand AgentCall where
  parseUserCommand = Just . AgentCall

instance IsCommand AgentCall where
  type Result AgentCall = ()
  runCommand agentId (AgentCall userInput) = do
    agentOutput <- step agentId $ CallAgent.UserCall userInput
    drainTools agentOutput
    pure ((), [])
   where
    -- Run the tools the model requested, record their outputs as events, and
    -- feed the results back to the model, repeating until it stops requesting
    -- tools. The original user input is kept unchanged for the whole drain.
    drainTools output = case output.toolRequests of
      []    -> pure ()
      tools -> do
        outcomes <- forM tools \tool -> do
          checkPermission tool
          toolOutput <- runToolForModel tool
          writeSection System Info $ into @Text (toolName' tool) <> " tool output:\n" <> toolOutput
          pure $ ToolResult tool toolOutput
        recordEvents agentId $ from <$> outcomes
        output' <- step agentId $ CallAgent.ToolResults userInput outcomes
        drainTools output'

instance Aeson.ToJSON AgentCall where
  toJSON (AgentCall userInput) =
    Aeson.object [("user_input", Aeson.toJSON userInput)]

instance IsEvent AgentCall where
  eventName _ = "AgentCall"
