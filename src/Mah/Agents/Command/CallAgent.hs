module Mah.Agents.Command.CallAgent where

import Control.Monad.Extra (whenJust)
import Control.Monad.Hefty (Eff, (:>))
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
import Mah.Agents.Data.Event (Event, IsEvent)
import Mah.Agents.Data.HarnessState (HarnessState(..))
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Data.UserInput (UserInput)
import Mah.Agents.Effect.Agent (callAgent)
import Mah.Agents.Effect.Console (writeSection)
import Mah.Agents.Effect.File (File, readFile)
import Mah.Agents.Pretty (Pretty(..))
import Witch (from, into)

data CallAgent = CallAgent UserInput

instance IsCommand CallAgent where
  type Result CallAgent = ()
  runCommand agentId (CallAgent userInput) = do
    agentInput <- mkAgentInput agentId userInput
    writeSection System Debug $ "Agent input:\n" <> pretty agentInput
    agentOutput <- callAgent agentInput
    writeSection Assistant Debug $ "Raw model output:\n" <> pretty agentOutput.raw
    writeSection Assistant Meta $ pretty $ into @AgentOutputMeta agentOutput
    whenJust agentOutput.thinking $ writeSection Assistant Thinking . pretty
    writeSection Assistant Info $ pretty $ into @AgentOutputInfo agentOutput
    pure ((), [from agentOutput :: Event])

instance IsEvent CallAgent

instance Pretty CallAgent where
  pretty (CallAgent userInput) = "Call agent with " <> pretty userInput

-- | Input to an agent derived from the harness state.
mkAgentInput
  :: (Ask Config :> es, File :> es, State HarnessState :> es)
  => AgentId -> UserInput -> Eff es AgentInput
mkAgentInput agentId userInput = do
  Config{modelName, soulPath} <- ask
  HarnessState{..} <- get
  modelSoul <- maybe (pure Nothing) readFile soulPath
  pure AgentInput
    { events = fromMaybe [] $ Map.lookup agentId events
    , modelTasks = fromMaybe [] $ Map.lookup agentId tasks
    , ..
    }
