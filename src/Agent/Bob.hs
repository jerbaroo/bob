module Agent.Bob where

import Agent.Bob.Prelude hiding (readFile)
import Agent.Bob.Command (Command(..), step)
import Agent.Bob.Command.CallAgent (mkAgentInput)
import Agent.Bob.Command.ReadConsole (ReadConsole(..))
import Agent.Bob.Config (loadConfig)
import Agent.Bob.Data.Config (ConfigF(..))
import Agent.Bob.Data.AgentId (AgentId(..))
import Agent.Bob.Data.HarnessState (HarnessState(..), mkHarnessState)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Agent (runAgent)
import Agent.Bob.Effect.Clock (runClock)
import Agent.Bob.Effect.Console (runConsole, writeSection)
import Agent.Bob.Effect.File (readFileLogErr, runFile)
import Agent.Bob.Effect.Harness (Harness)
import Agent.Bob.Effect.File (doesDirectoryExist, listDirectory)
import Agent.Bob.Effect.Permission (runPermissionAsk, runPermissionDeny)
import Agent.Bob.Permissions (readPermissions, renderPermissions)
import Agent.Bob.Prompt.Build (renderContextBreakdown)
import Agent.Bob.Session (branchSessions, newSessionPath, restoreSession)
import Agent.Bob.Skill (getSkills)
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.UserCommand.Registry (parseCommand)
import Control.Monad (filterM)
import Data.Effect.State (evalStateIORef)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import System.FilePath (takeFileName)

-- | Point the user at @/init@ when any referenced config file is missing.
checkInitFiles :: Harness es => Eff es ()
checkInitFiles = do
  Config{configPath, permissionsPath, soulPath} <- ask
  let paths = catMaybes [Just configPath, Just permissionsPath, soulPath]
  missing <- filterM (fmap isNothing . readFileLogErr) paths
  unless (null missing) $ writeSection System Info $
       "Missing config files: " <> Text.intercalate ", " missing
    <> ".\nRun /init to create them with default contents."

-- | Scan the configured skills directories, store the result in state, and
-- print what was found.
loadSkills :: Harness es => Eff es ()
loadSkills = do
  found <- getSkills
  writeSection System Info $ case found of
    [] -> "No skills found."
    _  -> "Skills:\n" <> pretty found

main :: IO ()
main = do
  config <- loadConfig
  sessionPath <- newSessionPath config
  let initialState = mkHarnessState config sessionPath
  let runPermission = if config.noAsk then runPermissionDeny else runPermissionAsk
  runEff
    . runAsk config
    . runAgent
    . runConsole
    . runClock
    . runFile (from config.projectRoot)
    . evalStateIORef initialState
    . runPermission
    $ do
      let agentId = AgentId 0
      startup agentId
      run agentId

-- | The main agentic loop: read user input, parse it into a command, execute.
run :: Harness es => AgentId -> Eff es ()
run agentId = do
  userInput <- step agentId ReadConsole
  whenJust (parseCommand userInput) \(Command command) ->
    void $ step agentId command
  run agentId

-- | Announce this run's session and list this branch's previous ones, so the
-- user knows what @/resume@ can reach.
sessionInfo :: Harness es => Eff es ()
sessionInfo = do
  Config{gitBranch, memoryPath, projectRoot} <- ask
  sessionPath <- gets (.sessionPath)
  writeSection System Info $ "Session: " <> sessionPath
  previous <- doesDirectoryExist memoryPath >>= \case
    False -> pure []
    True  ->
      branchSessions (from $ takeFileName $ from @Text projectRoot) gitBranch
        <$> listDirectory memoryPath
  unless (null previous) $ writeSection System Info $
       "Previous sessions (newest first):\n" <> pretty (take 5 previous)
    <> "\nUse /resume <name> to continue one."

startup :: Harness es => AgentId -> Eff es ()
startup agentId = do
  writeSection System Info
    . ("Hi. I'm Bob. Type /help for assistance.\n\n" <>) . pretty =<< ask
  checkInitFiles
  readPermissions >>= writeSection System Info . renderPermissions
  sessionInfo
  restoreSession agentId =<< gets (.sessionPath)
  -- Tasks restored with the session history, so the user sees what was
  -- still pending when the previous run ended.
  restoredTasks <- gets $ fromMaybe [] . Map.lookup agentId . (.tasks)
  unless (null restoredTasks) $ writeSection System Info $
    "Active tasks:\n" <> pretty restoredTasks
  loadSkills
  -- What a model call would send right now, so the user sees the size of
  -- the restored context before the first call.
  mkAgentInput agentId >>= writeSection System Info . renderContextBreakdown
