module Mah.Agents where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Control.Monad.Extra (whenJust)
import Control.Monad.Hefty (Eff, (:>), runEff)
import Data.Effect.Reader (Ask, ask, runAsk)
import Data.Effect.State (State, evalStateIORef, get, modify)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Prelude hiding (read)
import Mah.Agents.Data.AgentId (AgentId(..))
import Mah.Agents.Data.AgentInput (AgentInput(..))
import Mah.Agents.Data.AgentOutput (AgentOutput(..), AgentOutputInfo, AgentOutputMeta)
import Mah.Agents.Data.Command (Command(..))
import Mah.Agents.Data.Command qualified as Command
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.Event (Event(..), IsEvent(..))
import Mah.Agents.Data.HarnessState (HarnessState(..), mkHarnessState)
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.UserInput (UserInput(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Effect.Agent (Agent(..), callAgent, runAgent)
import Mah.Agents.Effect.Console (Console, readUserInput, runConsole, writeSection)
import Mah.Agents.Pretty (Pretty(..))
import Safe (lastMay)
import Witch (from, into)

-- | Input to an agent derived from the harness state.
mkAgentInput
  :: (Ask Config :> es, State HarnessState :> es)
  => AgentId -> UserInput -> Eff es AgentInput
mkAgentInput agentId userInput = do
  Config{model} <- ask
  HarnessState{..} <- get
  pure AgentInput
    { events = fromMaybe [] $ Map.lookup agentId events
    , tasks = fromMaybe [] $ Map.lookup agentId tasks
    , ..
    }

-- | Update the harness state with a new event.
recordEvents
  :: (Ask Config :> es, Console :> es, State HarnessState :> es)
  => AgentId -> [Event] -> Eff es ()
recordEvents agentId newEvents = do
  modify $ \HarnessState{..} -> HarnessState
    { events =
        let f Nothing = Just newEvents
            f (Just events') = Just $ newEvents <> events'
        in Map.alter f agentId events
    , tasks = maybe tasks (Map.singleton agentId) $
        eventTasks =<< lastMay newEvents
    , ..
    }
  forM_ newEvents $ writeSection System Meta . ("Recorded event: " <>) . pretty

run
  :: (Agent :> es, Ask Config :> es, Console :> es, State HarnessState :> es)
  => AgentId -- ^ The agent in-focus.
  -> Command -- ^ The command to run.
  -> Eff es ()
run agentId command = do
  -- Run the command.
  (event, nextCommand) <- runCommand agentId command
  -- Record the command and the resulting event.
  recordEvents agentId [from command, event]
  -- Run the next command.
  run agentId nextCommand

runCommand
  :: (Agent :> es, Ask Config :> es, Console :> es, State HarnessState :> es)
  => AgentId -- ^ The agent in-focus.
  -> Command -- ^ The command to run.
  -> Eff es (Event, Command)
runCommand _agentId Command.ReadConsole =
  (from &&& Command.CallAgent) <$> readUserInput

runCommand agentId (Command.CallAgent userInput) = do
  agentOutput <- callAgent =<< mkAgentInput agentId userInput
  writeSection Assistant Debug $ "Raw model output:\n" <> pretty agentOutput.raw
  writeSection Assistant Meta $ pretty $ into @AgentOutputMeta agentOutput
  whenJust agentOutput.thinking $ writeSection Assistant Thinking . pretty
  writeSection Assistant Info $ pretty $ into @AgentOutputInfo agentOutput
  pure $ (from agentOutput, Command.ReadConsole)

runCommand agentId command = do
  writeSection System Info $ "Command not supported: " <> pretty command
  runCommand agentId Command.ReadConsole

main :: IO ()
main = do
  let config = Config
        { level = Debug
        , model = "Qwen"
        , session = "./mah-agents/sessions/default"
        , temperature = 0.5
        , timeoutSeconds = 10 * 60 -- 10 minutes.
        , url = "http://127.0.0.1:8080/v1/chat/completions"
        }
  let initialState = mkHarnessState config
  runEff . runAsk config . runAgent . runConsole . evalStateIORef initialState $ do
    writeSection System Info $ "Type /help for assistance.\n\n" <> pretty config
    run (AgentId 0) Command.ReadConsole
