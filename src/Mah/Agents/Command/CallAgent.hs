module Mah.Agents.Command.CallAgent where

import Control.Monad.Extra (whenJust)
import Control.Monad.Hefty (Eff, (:>))
import Data.Aeson qualified as Aeson
import Data.Effect.Reader (Ask, ask)
import Data.Effect.State (State, get)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Prelude hiding (readFile)
import Mah.Agents.Command (IsCommand(..))
import Mah.Agents.Data.AgentId (AgentId)
import Mah.Agents.Data.AgentInput (AgentInput(..))
import Mah.Agents.Data.AgentOutput (AgentOutput(..), AgentOutputInfo, AgentOutputMeta)
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.Event (IsEvent(..))
import Mah.Agents.Data.HarnessState (HarnessState(..))
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Data.ToolResult (ToolResult)
import Mah.Agents.Data.UserInput (UserInput)
import Mah.Agents.Effect.Agent (callAgent)
import Mah.Agents.Effect.Console (writeSection)
import Mah.Agents.Effect.File (File, readFile)
import Mah.Agents.Pretty (Pretty(..))
import Mah.Agents.Prompt.Build (buildPrompt)
import Witch (from, into)

data CallAgent
  = ToolResults UserInput [ToolResult]
  | UserCall UserInput

instance IsCommand CallAgent where
  type Result CallAgent = AgentOutput
  runCommand agentId call = do
    agentInput <- mkAgentInput agentId call
    writeSection System Debug $ "Prompt sent to model:\n" <> pretty (buildPrompt agentInput)
    agentOutput <- callAgent agentInput
    writeSection Assistant Debug $ "Raw output from model:\n" <> pretty agentOutput.raw
    writeSection Assistant Meta $ pretty $ into @AgentOutputMeta agentOutput
    whenJust agentOutput.thinking $ writeSection Assistant Thinking . pretty
    writeSection Assistant Info $ pretty $ into @AgentOutputInfo agentOutput
    pure (agentOutput, [from agentOutput])

instance Aeson.ToJSON CallAgent where
  toJSON (ToolResults userInput results) = Aeson.object
    [ ("user_input", Aeson.toJSON userInput)
    , ("tool_results", Aeson.toJSON results)
    ]
  toJSON (UserCall userInput) =
    Aeson.object [("user_input", Aeson.toJSON userInput)]

instance IsEvent CallAgent where
  eventName _ = "CallAgent"

-- | Input to an agent derived from the harness state.
mkAgentInput
  :: (Ask Config :> es, File :> es, State HarnessState :> es)
  => AgentId -> CallAgent -> Eff es AgentInput
mkAgentInput agentId call = do
  Config{modelName, soulPath} <- ask
  HarnessState{..} <- get
  modelSoul <- maybe (pure Nothing) readFile soulPath
  pure AgentInput
    { events = fromMaybe [] $ Map.lookup agentId events
    , modelTasks = fromMaybe [] $ Map.lookup agentId tasks
    , userInput = case call of
        (UserCall userInput)      -> userInput
        (ToolResults userInput _) -> userInput
    , toolResults = case call of
        (UserCall _)            -> []
        (ToolResults _ results) -> results
    , ..
    }
