module Mah.Agents where

import Control.Monad (forM, forM_)
import Control.Monad.Hefty (Eff, (:>), runEff)
import Data.Aeson qualified as Aeson
import Data.Effect.Reader (Ask, asks, runAsk)
import Data.Effect.State (State, evalStateIORef, modify)
import Data.Map qualified as Map
import Prelude hiding (appendFile, readFile)
import Mah.Agents.Command (Command(..), Harness, IsCommand(..))
import Mah.Agents.Command.CallAgent qualified as CallAgent
import Mah.Agents.Command.ReadConsole (ReadConsole(..))
import Mah.Agents.Data.AgentId (AgentId(..))
import Mah.Agents.Data.AgentOutput (AgentOutput(..))
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.ToolResult (ToolResult(..))
import Mah.Agents.Data.Event (Event, IsEvent(..))
import Mah.Agents.Data.HarnessState (HarnessState(..), mkHarnessState)
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Data.UserInput (UserInput)
import Mah.Agents.Effect.Agent (runAgent)
import Mah.Agents.Effect.Console (Console, runConsole, writeSection)
import Mah.Agents.Effect.File (File, appendFile, runFile)
import Mah.Agents.Pretty (Pretty(..), compact)
import Mah.Agents.Tool (runToolForModel, toolName')
import Safe (lastMay)
import Witch (from)

-- | Update the harness state with new events.
recordEvents
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
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
  memoryPath <- asks (.memoryPath)
  forM_ newEvents $ \event -> do
    writeSection System Meta $ "Recorded event: " <> eventName event
    appendFile memoryPath $ compact event <> "\n"

-- | The main agentic loop.
run :: Harness es => AgentId -> Eff es ()
run agentId = do
  userInput <- step ReadConsole
  agentOutput <- step $ CallAgent.UserCall userInput
  drainTools userInput agentOutput
  run agentId
 where
  -- | Run the tools the model requested, record their outputs as events, and
  -- feed the results back to the model, repeating until it stops requesting
  -- tools. The original user input is kept unchanged for the whole drain.
  drainTools :: Harness es => UserInput -> AgentOutput -> Eff es ()
  drainTools userInput output = case output.toolRequests of
    []    -> pure ()
    tools -> do
      outcomes <- forM tools \tool -> do
        toolOutput <- runToolForModel tool
        writeSection System Info $ toolName' tool <> " tool output:\n" <> toolOutput
        pure $ ToolResult tool toolOutput
      recordEvents agentId $ from <$> outcomes
      output' <- step $ CallAgent.ToolResults userInput outcomes
      drainTools userInput output'

  -- | Run a command, record the events, and return the command result.
  step :: (Aeson.ToJSON c, Harness es, IsCommand c, IsEvent c) => c -> Eff es (Result c)
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
