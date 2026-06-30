module Mah.Agents where

import Control.Monad.Hefty
import Data.Effect.Reader (runAsk)
import Data.Either.Extra (maybeToEither)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prelude hiding (read)
import Mah.Agents.Config (Config(..))
import Mah.Agents.State.AgentId (AgentId(..))
import Mah.Agents.State.Command (Command(..))
import Mah.Agents.State.Command qualified as Command
import Mah.Agents.State.Event (Event(..))
import Mah.Agents.Effect.Agent (Agent(..), runAgent)
import Mah.Agents.Effect.Console (Console, readPrompt, runConsole, writeConsoleLn)
import Mah.Agents.State.AgentInput (AgentInput(..))
import Mah.Agents.State.HarnessState (HarnessState(..), emptyHarnessState)
import Mah.Agents.Pretty (Pretty(..))
import Witch (from)

-- | Input to an agent derived from the harness state.
agentInput :: AgentId -> Text -> HarnessState -> Either Text AgentInput
agentInput agentId userInput HarnessState{..} = do
  tasks' <- maybeToEither ("Task for " <> pretty agentId <> " not found") $
    Map.lookup agentId tasks
  let agentEvents = fromMaybe [] $ Map.lookup agentId events
  pure AgentInput
    { events = agentEvents
    , tasks = tasks'
    , userInput
    }

-- | Update the harness state with a new event.
recordEvent :: AgentId -> Event -> HarnessState -> HarnessState
recordEvent agentId event HarnessState{..} =
  let state' = HarnessState
        { events =
            let f Nothing = Just [event]
                f (Just events') = Just $ event : events'
            in Map.alter f agentId events
        , tasks
        }
  in case event of
    Event_AgentOutput{} -> state' { tasks = Map.delete agentId tasks }
    _ -> state'

run
  :: (Agent :> es, Console :> es)
  => AgentId -- ^ The agent in-focus.
  -> Command -- ^ The command to run.
  -> HarnessState -- ^ The current state.
  -> Eff es ()
run agentId command harnessState = do
  -- Run the command.
  (event, nextCommand) <- runCommand agentId command harnessState
  -- Record the command run, and the resulting event.
  let nextHarnessState = recordEvent agentId event
        $ recordEvent agentId (Event_CommandRun command) harnessState
  -- Run the next command.
  run agentId nextCommand nextHarnessState

runCommand
  :: (Agent :> es, Console :> es)
  => AgentId -- ^ The agent in-focus.
  -> Command -- ^ The command to run.
  -> HarnessState -- ^ The current state.
  -> Eff es (Event, Command)
runCommand agentId Command.ReadConsole _ =
  (, Command.CallAgent agentId) . Event_UserInput <$> readPrompt
runCommand agentId command harnessState = do
  writeConsoleLn $ "Command not supported: " <> from (show command)
  runCommand agentId Command.ReadConsole harnessState

main :: IO ()
main = do
  let config = Config "foo" "bar"
  runEff . runAsk config . runAgent . runConsole  $ do
    writeConsoleLn $ "Config:" <> pretty config
    writeConsoleLn "Welcome. How may I help you?"
    run (AgentId 0) Command.ReadConsole emptyHarnessState
