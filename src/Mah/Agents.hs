module Mah.Agents where

import Control.Monad (forM_)
import Control.Monad.Hefty (Eff, (:>), runEff)
import Data.Effect.Reader (Ask, runAsk)
import Data.Effect.State (State, evalStateIORef, modify)
import Data.Map qualified as Map
import Prelude hiding (readFile)
import Mah.Agents.Command (Command(..), Harness, IsCommand(..))
import Mah.Agents.Command.CallAgent (CallAgent(..))
import Mah.Agents.Command.ReadConsole (ReadConsole(..))
import Mah.Agents.Data.AgentId (AgentId(..))
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.Event (Event, IsEvent(..))
import Mah.Agents.Data.HarnessState (HarnessState(..), mkHarnessState)
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Effect.Agent (runAgent)
import Mah.Agents.Effect.Console (Console, runConsole, writeSection)
import Mah.Agents.Effect.File (runFile)
import Mah.Agents.Pretty (Pretty(..))
import Safe (lastMay)
import Witch (from)

-- | Update the harness state with new events.
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

-- | The main agentic loop.
run :: Harness es => AgentId -> Eff es ()
run agentId = do
  userInput <- step ReadConsole
  step $ CallAgent userInput
  run agentId
 where
  step :: (Harness es, IsCommand c, IsEvent c, Pretty c) => c -> Eff es (Result c)
  step command = do
    (result, events) <- runCommand agentId command
    recordEvents agentId $ from (Command command) : events
    pure result

main :: IO ()
main = do
  let config = Config
        { engineUrl = "http://127.0.0.1:8080/v1/chat/completions"
        , logLevel = Debug
        , memoryPath = "~/.mah-agents/sessions/default"
        , modelName = "Qwen"
        , modelTemperature = 0.5
        , soulPath = Just "~/.config/mah-agents/SOUL.md"
        , timeoutSeconds = 10 * 60 -- 10 minutes.
        }
  let initialState = mkHarnessState config
  runEff . runAsk config . runAgent . runConsole . runFile . evalStateIORef initialState $ do
    writeSection System Info $ "Type /help for assistance.\n\n" <> pretty config
    run $ AgentId 0
