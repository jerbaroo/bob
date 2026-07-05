module Agent.Bob.Command.CallAgent where

import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.AgentId (AgentId)
import Agent.Bob.Data.AgentInput (AgentInput(..))
import Agent.Bob.Data.AgentOutput (AgentMessageContent(..), AgentOutput(..))
import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Agent (callAgent)
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Effect.File (File, readFile)
import Agent.Bob.Prelude hiding (readFile)
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.Prompt.Build (buildPrompt)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map

-- | Call the agent with a prompt built from the recorded events.
data CallAgent = CallAgent

instance Aeson.ToJSON CallAgent where
  toJSON CallAgent = "CallAgent"

instance IsCommand CallAgent where
  type Result CallAgent = AgentOutput
  runCommand agentId CallAgent = do
    agentInput <- mkAgentInput agentId
    writeSection System Debug $ "Prompt sent to model:\n" <> pretty (buildPrompt agentInput)
    agentOutput@AgentOutput{..} <- callAgent agentInput
    writeSection Assistant Debug $ "Raw output from model:\n" <> pretty raw
    writeSection Assistant Meta $ pretty agentOutputMeta
    whenJust message.thinking $ writeSection Assistant Thinking . pretty
    writeSection Assistant Info $ from agentMessageContent.resolution <> case agentMessageContent.tasks of
      [] -> ""
      _  -> "\n\nRemaining tasks:\n" <> pretty agentMessageContent.tasks
    pure (agentOutput, [from agentOutput])

instance IsEvent CallAgent where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

-- | Input to an agent derived from the harness state.
mkAgentInput
  :: (Ask Config :> es, File :> es, State HarnessState :> es)
  => AgentId -> Eff es AgentInput
mkAgentInput agentId = do
  Config{modelName, soulPath} <- ask
  HarnessState{..} <- get
  modelSoul <- maybe (pure Nothing) readFile soulPath
  pure AgentInput
    { events = fromMaybe [] $ Map.lookup agentId events
    , modelTasks = fromMaybe [] $ Map.lookup agentId tasks
    , ..
    }
