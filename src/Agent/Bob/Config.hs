module Agent.Bob.Config where

import Agent.Bob.Config.Cli qualified as Cli
import Agent.Bob.Config.File qualified as File
import Agent.Bob.Data.Config (Config, ConfigF(..), applyPartial, defaultConfigPath, globalConfigPath)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Effect.Console (runConsole)
import Agent.Bob.Effect.File (readFileMaybe, runFile)
import Agent.Bob.Prelude
import Agent.Bob.ProjectRoot (agentBobDir, agentsDir, findProjectRoot, sessionKey)
import Data.Text qualified as Text
import Options.Applicative (execParser)
import System.Directory (getCurrentDirectory)
import System.Exit (die)
import System.FilePath ((</>))

-- | The defaults, overridden by the config files, overridden by the CLI.
-- Takes the project root (see 'findProjectRoot') and the session key (see
-- 'sessionKey'). Relative paths are resolved against the project root by the
-- File effect, so episodic memory lives in the project, keyed by branch.
defaultConfig :: FilePath -> Text -> Config
defaultConfig root key = Config
  { configPath = defaultConfigPath
  , engineUrl = "http://127.0.0.1:8080/v1/chat/completions"
  , gitBranch = key
  , logLevel = Debug
  , memoryPath = from $ agentBobDir </> "sessions" </> from key
  , modelName = "Qwen"
  , modelTemperature = 0.5
  , noAsk = False
  , permissionsPath = from $ agentBobDir </> "permissions"
  , projectRoot = from root
  , skillsPaths = [from $ agentsDir </> "skills"]
  , soulPath = Just $ from $ agentBobDir </> "SOUL.md"
  , timeoutSeconds = 10 * 60 -- 10 minutes.
  }

-- | Read the effective configuration with the precedence
-- CLI > project file > global file > default.
loadConfig :: IO Config
loadConfig = do
  cwd <- getCurrentDirectory
  -- Not inside a git repository: treat the CWD as the project root.
  root <- fromMaybe cwd <$> findProjectRoot cwd
  key <- sessionKey root
  cliPartial <- execParser Cli.parserInfo
  -- Reuse the File effect so config paths get the same tilde-expansion,
  -- root-relative resolution and missing-file handling as everything else.
  let readPartial path = do
        rawConfig <- runEff . runConsole . runFile root $ readFileMaybe path
        either (die . Text.unpack) pure $
          maybe (Right mempty) File.parseConfig rawConfig
  globalPartial <- readPartial globalConfigPath
  projectPartial <- readPartial $
    fromMaybe defaultConfigPath cliPartial.configPath
  pure $ applyPartial (cliPartial <> projectPartial <> globalPartial) $
    defaultConfig root key
