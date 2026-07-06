module Agent.Bob.UserCommand.AgentCall where

import Data.Aeson qualified as Aeson
import Agent.Bob.Command (IsCommand(..), recordEvents, step)
import Agent.Bob.Command.CallAgent qualified as CallAgent
import Agent.Bob.Data.AgentOutput (AgentMessageContent(..), AgentOutput(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.ToolResult (ToolResult(..))
import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Permissions (checkPermission)
import Agent.Bob.Prelude
import Agent.Bob.Tool (runToolForModel, toolName')
import Agent.Bob.UserCommand (IsUserCommand(..))

-- | Call the agent with the user's input and run any tools it requests. This
-- is the fallback user command: it parses any input, so in the registry it
-- must be tried last.
newtype AgentCall = AgentCall UserInput

instance Aeson.ToJSON AgentCall where
  toJSON (AgentCall userInput) =
    Aeson.object [("user_input", Aeson.toJSON userInput)]

instance IsCommand AgentCall where
  type Result AgentCall = ()
  runCommand agentId (AgentCall _userInput) = do
    agentOutput <- step agentId CallAgent.CallAgent
    drainTools agentOutput
    pure ((), [])
   where
    -- Run the tools the model requested, record their outputs as events, and
    -- call the agent again, repeating until it stops requesting tools. The
    -- user input is already recorded as an event, so each call sees it.
    drainTools output = case output.agentMessageContent.toolRequests of
      []    -> pure ()
      tools -> do
        outcomes <- forM tools \tool -> do
          let name = into @Text $ toolName' tool
          toolOutput <- checkPermission tool >>= \case
            -- Denied: surface the reason to the model as the tool output so it
            -- can adapt, rather than crashing the harness.
            False -> do
              let reason = "No permission for tool " <> name
              writeSection System Error reason
              pure reason
            True -> do
              result <- runToolForModel tool
              writeSection System Info $ name <> " tool output:\n" <> result
              pure result
          pure $ ToolResult (toolName' tool) toolOutput
        recordEvents agentId $ from <$> outcomes
        output' <- step agentId CallAgent.CallAgent
        drainTools output'

instance IsEvent AgentCall where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand AgentCall where
  parseUserCommand = Just . AgentCall
  userCommandDescription =
    "Send the input to the agent and run any tools it requests."
  userCommandName = []
