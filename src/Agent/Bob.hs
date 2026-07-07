module Agent.Bob where

import Agent.Bob.Prelude hiding (readFile)
import Agent.Bob.Command (Command(..), Harness, step)
import Agent.Bob.Command.CallAgent (mkAgentInput)
import Agent.Bob.Command.ReadConsole (ReadConsole(..))
import Agent.Bob.Config (loadConfig)
import Agent.Bob.Data.Config (ConfigF(..))
import Agent.Bob.Data.AgentId (AgentId(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Event.Registry (LineResult(..), readEventLine)
import Agent.Bob.Data.HarnessState (HarnessState(..), mkHarnessState)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Agent (runAgent)
import Agent.Bob.Effect.Clock (runClock)
import Agent.Bob.Effect.Console (runConsole, writeSection)
import Agent.Bob.Effect.File (readFileLogErr, runFile)
import Agent.Bob.Permissions (readPermissions, renderPermissions)
import Agent.Bob.Prompt.Build (renderContextBreakdown)
import Agent.Bob.Skill (getSkills)
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
  contents <- readFileLogErr path
  -- Classify each non-blank line. Malformed lines are genuine corruption worth
  -- reporting; Skipped lines (commands and other non-restored events) are
  -- expected, so they must not be counted as errors.
  let results = map readEventLine $ filter (not . Text.null . Text.strip) $
        maybe [] Text.lines contents
      restored = [e | Restored e <- results]
      malformed = length [() | Malformed <- results]
  modify $ \HarnessState{..} -> HarnessState
    { events = Map.insert agentId (reverse restored) events
    , tasks = maybe tasks (Map.singleton agentId) $
        lastMay (mapMaybe eventTasks restored)
    , ..
    }
  writeSection System Info $ case length restored of
    0 -> "No session history loaded."
    1 -> "Loaded session history (1 entry)."
    n -> "Loaded session history (" <> pretty n <> " entries)."
  when (malformed > 0) $ writeSection System Error $
    "Skipped " <> pretty malformed <> " malformed session-history line(s)."

-- | Scan the configured skills directories, store the result in state, and
-- print what was found.
loadSkills :: Harness es => Eff es ()
loadSkills = do
  found <- getSkills
  writeSection System Info $ case found of
    [] -> "No skills found."
    _  -> "Skills:\n" <> pretty found

-- | Point the user at @/init@ when any referenced config file is missing.
checkInitFiles :: Harness es => Eff es ()
checkInitFiles = do
  Config{configPath, permissionsPath, soulPath} <- ask
  let paths = catMaybes [Just configPath, Just permissionsPath, soulPath]
  missing <- filterM (fmap isNothing . readFileLogErr) paths
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
  runEff . runAsk config . runAgent . runConsole . runClock . runFile (from config.projectRoot) . evalStateIORef initialState $ do
    writeSection System Info $ "Hi. I'm Bob. Type /help for assistance.\n\n" <> pretty config
    checkInitFiles
    readPermissions >>= writeSection System Info . renderPermissions
    let agentId = AgentId 0
    restoreSession agentId
    -- Tasks restored with the session history, so the user sees what was
    -- still pending when the previous run ended.
    restoredTasks <- gets $ fromMaybe [] . Map.lookup agentId . (.tasks)
    unless (null restoredTasks) $ writeSection System Info $
      "Active tasks:\n" <> pretty restoredTasks
    loadSkills
    -- What a model call would send right now, so the user sees the size of
    -- the restored context before the first call.
    mkAgentInput agentId >>= writeSection System Info . renderContextBreakdown
    run agentId
