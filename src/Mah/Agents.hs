module Mah.Agents where

import Control.Monad.Extra (whenJust)
import Control.Monad.Hefty (Eff, (:>), runEff)
import Data.Effect.Reader (runAsk)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Prelude hiding (read)
import Mah.Agents.Data.AgentId (AgentId(..))
import Mah.Agents.Data.AgentInput (AgentInput(..))
import Mah.Agents.Data.AgentOutput (AgentOutput(..))
import Mah.Agents.Data.Command (Command(..))
import Mah.Agents.Data.Command qualified as Command
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.Event (Event(..), IsEvent(..))
import Mah.Agents.Data.HarnessState (HarnessState(..), mkHarnessState)
import Mah.Agents.Data.UserInput (UserInput(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Effect.Agent (Agent(..), callAgent, runAgent)
import Mah.Agents.Effect.Console (Console, readUserInput, runConsole, writeSection)
import Mah.Agents.Pretty (Pretty(..))
import Witch (from)

-- | Input to an agent derived from the harness state.
mkAgentInput :: AgentId -> UserInput -> HarnessState -> AgentInput
mkAgentInput agentId userInput HarnessState{..} =
  AgentInput
    { events = fromMaybe [] $ Map.lookup agentId events
    , model = "Qwen"
    , tasks = fromMaybe [] $ Map.lookup agentId tasks
    , temperature = 0.7
    , userInput
    }

-- | Update the harness state with a new event.
recordEvent :: AgentId -> Event -> HarnessState -> HarnessState
recordEvent agentId event HarnessState{..} =
  HarnessState
        { events =
            let f Nothing = Just [event]
                f (Just events') = Just $ event : events'
            in Map.alter f agentId events
        , tasks = maybe tasks (Map.singleton agentId) $ eventTasks event
        , ..
        }

run
  :: (Agent :> es, Console :> es)
  => AgentId -- ^ The agent in-focus.
  -> Command -- ^ The command to run.
  -> HarnessState -- ^ The current state.
  -> Eff es ()
run agentId command harnessState = do
  -- Run the command.
  (event, nextCommand) <- runCommand agentId command harnessState
  -- Record the command, and the resulting event.
  let nextHarnessState = recordEvent agentId event
        $ recordEvent agentId (from command) harnessState
  -- Run the next command.
  run agentId nextCommand nextHarnessState

runCommand
  :: (Agent :> es, Console :> es)
  => AgentId -- ^ The agent in-focus.
  -> Command -- ^ The command to run.
  -> HarnessState -- ^ The current state.
  -> Eff es (Event, Command)
runCommand _agentId Command.ReadConsole _ =
  readUserInput <&> \userInput ->
    (from userInput, Command.CallAgent userInput)
runCommand agentId (Command.CallAgent userInput) harnessState = do
  agentOutput <- callAgent (mkAgentInput agentId userInput harnessState)
  whenJust agentOutput.thinking $ writeSection Assistant . from . pretty
  pure $ (from agentOutput, Command.ReadConsole)
runCommand agentId command harnessState = do
  writeSection System $ "Command not supported: " <> from (show command)
  runCommand agentId Command.ReadConsole harnessState

main :: IO ()
main = do
  let config = Config 0.7 "http://127.0.0.1:8080/v1/chat/completions"
  runEff . runAsk config . runAgent . runConsole  $ do
    writeSection System $ pretty config
    run (AgentId 0) Command.ReadConsole $ mkHarnessState config
