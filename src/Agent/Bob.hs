module Agent.Bob where

import Agent.Bob.Prelude hiding (readFile)
import Agent.Bob.Command (Command(..), Harness, step)
import Agent.Bob.Command.ReadConsole (ReadConsole(..))
import Agent.Bob.Config (loadConfig)
import Agent.Bob.Data.Config (ConfigF(..))
import Agent.Bob.Data.AgentId (AgentId(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Event.Registry (parseEventLine)
import Agent.Bob.Data.HarnessState (HarnessState(..), mkHarnessState)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Agent (runAgent)
import Agent.Bob.Effect.Console (runConsole, writeSection)
import Agent.Bob.Effect.File (readFile, runFile)
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.UserCommand.Registry (parseCommand)
import Control.Monad (filterM)
import Data.Effect.State (evalStateIORef)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Text qualified as Text

-- | Restore the session from the persisted event log, so the model starts
-- with the context of previous runs instead of starting fresh. Lines that
-- don't parse as a known event are skipped.
restoreSession :: Harness es => AgentId -> Eff es ()
restoreSession agentId = do
  path <- asks (.memoryPath)
  contents <- readFile path
  -- TODO log an error if any events failed to parse.
  let restored = maybe [] (mapMaybe parseEventLine . Text.lines) contents
  modify $ \HarnessState{..} -> HarnessState
    { events = Map.insert agentId (reverse restored) events
    , tasks = maybe tasks (Map.singleton agentId) $
        lastMay (mapMaybe eventTasks restored)
    , ..
    }
  writeSection System Meta $
    "Restored " <> pretty (length restored) <> " events."

-- | Point the user at @/init@ when any referenced config file is missing.
checkInitFiles :: Harness es => Eff es ()
checkInitFiles = do
  Config{configPath, permissionsPath, soulPath} <- ask
  let paths = catMaybes [Just configPath, Just permissionsPath, soulPath]
  missing <- filterM (fmap isNothing . readFile) paths
  unless (null missing) $ writeSection System Info $
       "Missing config files: " <> Text.intercalate ", " missing
    <> ".\nRun /init to create them with default contents."

-- | The main agentic loop: read user input, parse it into a command, execute.
run :: Harness es => AgentId -> Eff es ()
run agentId = do
  userInput <- step agentId ReadConsole
  whenJust (parseCommand userInput) \(Command command) ->
    void $ step agentId command
  run agentId

main :: IO ()
main = do
  config <- loadConfig
  let initialState = mkHarnessState config
  runEff . runAsk config . runAgent . runConsole . runFile (from config.projectRoot) . evalStateIORef initialState $ do
    writeSection System Info $ "Hi. I'm Bob. Type /help for assistance.\n\n" <> pretty config
    checkInitFiles
    let agentId = AgentId 0
    restoreSession agentId
    run agentId
