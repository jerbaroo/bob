module Agent.Bob.UserCommand.AgentCall where

import Control.Monad (forM)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Agent.Bob.Command (IsCommand(..), recordEvents, step)
import Agent.Bob.Command.CallAgent qualified as CallAgent
import Agent.Bob.Data.AgentOutput (AgentOutput(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.ToolResult (ToolResult(..))
import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Permissions (checkPermission)
import Agent.Bob.Tool (runToolForModel, toolName')
import Agent.Bob.UserCommand (IsUserCommand(..))
import Witch (from, into)

-- | Call the agent with the user's input and run any tools it requests. This
-- is the fallback user command: it parses any input, so in the registry it
-- must be tried last.
newtype AgentCall = AgentCall UserInput

instance IsUserCommand AgentCall where
  parseUserCommand = Just . AgentCall
  userCommandDescription =
    "Send the input to the agent and run any tools it requests."
  userCommandName = []

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
