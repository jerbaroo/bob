module Agent.Bob.UserCommand.AgentCall where

import Data.Aeson qualified as Aeson
import Agent.Bob.Command (IsCommand(..), recordEvents, step)
import Agent.Bob.Command.CallAgent qualified as CallAgent
import Agent.Bob.Data.AgentOutput (AgentMessageContent(..), AgentOutput(..))
import Agent.Bob.Data.Event (IsEvent(..), KnownEvent)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Permission (Decision(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.ToolResult (ToolResult(..))
import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Effect.Permission (requestPermission)
import Agent.Bob.Permissions (checkPermission)
import Agent.Bob.Prelude
import Agent.Bob.Tool (runToolForModel, toolName', toolParam')
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
              run = do
                result <- runToolForModel tool
                writeSection System Info $ name <> " tool output:\n" <> result
                pure result
          -- Unpermitted: ask the user. A denial becomes the tool's output so
          -- the model can adapt, rather than crashing the harness.
          toolOutput <- checkPermission tool >>= \case
            True -> run
            False -> requestPermission (toolName' tool) (toolParam' tool) >>= \case
              Deny -> do
                let reason = "No permission for tool " <> name
                writeSection System Error reason
                pure reason
              _ -> run
          pure $ ToolResult (toolName' tool) toolOutput
        recordEvents agentId $ from <$> outcomes
        output' <- step agentId CallAgent.CallAgent
        drainTools output'

instance KnownEvent AgentCall

instance IsEvent AgentCall where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand AgentCall where
  parseUserCommand = Just . AgentCall
  userCommandDescription =
    "Send the input to the agent and run any tools it requests."
  userCommandName = []
