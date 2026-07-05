module Agent.Bob.Config where

import Agent.Bob.Config.Cli qualified as Cli
import Agent.Bob.Config.File qualified as File
import Agent.Bob.Data.Config (Config, ConfigF(..), applyPartial, defaultConfigPath, globalConfigPath)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Effect.File (readFile, runFile)
import Agent.Bob.Prelude hiding (readFile)
import Agent.Bob.ProjectRoot (findProjectRoot, projectDir)
import Data.Text qualified as Text
import Options.Applicative (execParser)
import System.Directory (getCurrentDirectory)
import System.Exit (die)
import System.FilePath ((</>))

-- | The defaults, overridden by the config files, overridden by the CLI.
-- Takes the project root (see 'findProjectRoot') and the CWD. Relative paths
-- are resolved against the project root by the File effect.
defaultConfig :: FilePath -> FilePath -> Config
defaultConfig root cwd = Config
  { configPath = defaultConfigPath
  , engineUrl = "http://127.0.0.1:8080/v1/chat/completions"
  , logLevel = Debug
  , memoryPath = sessionPath cwd
  , modelName = "Qwen"
  , modelTemperature = 0.5
  , permissionsPath = from $ projectDir </> "permissions"
  , projectRoot = from root
  , soulPath = Just $ from $ projectDir </> "SOUL.md"
  , timeoutSeconds = 10 * 60 -- 10 minutes.
  }

-- | Read the effective configuration with the precedence
-- CLI > project file > global file > default.
loadConfig :: IO Config
loadConfig = do
  cwd <- getCurrentDirectory
  -- Not inside a git repository: treat the CWD as the project root.
  root <- fromMaybe cwd <$> findProjectRoot cwd
  cliPartial <- execParser Cli.parserInfo
  -- Reuse the File effect so config paths get the same tilde-expansion,
  -- root-relative resolution and missing-file handling as everything else.
  let readPartial path = do
        rawConfig <- runEff . runFile root $ readFile path
        either (die . Text.unpack) pure $
          maybe (Right mempty) File.parseConfig rawConfig
  globalPartial <- readPartial globalConfigPath
  projectPartial <- readPartial $
    fromMaybe defaultConfigPath cliPartial.configPath
  pure $ applyPartial (cliPartial <> projectPartial <> globalPartial) $
    defaultConfig root cwd

-- | Sessions are stored centrally but keyed by the directory the tool is
-- started from, so each project keeps its own history.
sessionPath :: FilePath -> Text
sessionPath cwd = "~/.agent-bob/sessions/" <> sessionName cwd

sessionName :: String -> Text
sessionName cwd = case Text.uncons $ Text.replace "/" "-" $ from cwd of
  Just ('-', rest) -> rest
  _ -> error $ "Current working directory should begin with /"
