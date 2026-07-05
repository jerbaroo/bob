module Agent.Bob.Config where

import Agent.Bob.Config.Cli qualified as Cli
import Agent.Bob.Config.File qualified as File
import Agent.Bob.Data.Config (Config, ConfigF(..), applyPartial, defaultConfigPath)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Effect.File (readFile, runFile)
import Agent.Bob.Prelude hiding (readFile)
import Agent.Bob.ProjectRoot (findProjectRoot, projectDir)
import Data.Text qualified as Text
import Options.Applicative (execParser)
import System.Directory (getCurrentDirectory)
import System.Exit (die)
import System.FilePath ((</>))

-- | The default config, overridden by the config file, overridden by the CLI.
-- Takes the project root (see 'findProjectRoot') and the CWD.
defaultConfig :: FilePath -> FilePath -> Config
defaultConfig root cwd = Config
  { configPath = defaultConfigPath
  , engineUrl = "http://127.0.0.1:8080/v1/chat/completions"
  , logLevel = Debug
  , memoryPath = sessionPath cwd
  , modelName = "Qwen"
  , modelTemperature = 0.5
  , permissionsPath = "~/.agent-bob/permissions"
  , projectRoot = from root
  , soulPath = Just $ from $ root </> projectDir </> "SOUL.md"
  , timeoutSeconds = 10 * 60 -- 10 minutes.
  }

-- | Read the effective configuration with the precedence CLI > file > default.
-- CLI flags are parsed first: they take precedence and also choose which config
-- file to read (--config-path).
loadConfig :: IO Config
loadConfig = do
  cwd <- getCurrentDirectory
  -- Not inside a git repository: treat the CWD as the project root.
  root <- fromMaybe cwd <$> findProjectRoot cwd
  cliPartial <- execParser Cli.parserInfo
  let cfgFilePath = fromMaybe defaultConfigPath cliPartial.configPath
  -- Reuse the File effect so the config path gets the same tilde-expansion and
  -- missing-file handling (Nothing) as everything else.
  rawConfig <- runEff . runFile $ readFile cfgFilePath
  filePartial <- either (die . Text.unpack) pure $
    maybe (Right mempty) File.parseConfig rawConfig
  -- Precedence: CLI > file > default.
  pure $ applyPartial (cliPartial <> filePartial) $ defaultConfig root cwd

-- | Sessions are stored centrally but keyed by the directory the tool is
-- started from, so each project keeps its own history.
sessionPath :: FilePath -> Text
sessionPath cwd = "~/.agent-bob/sessions/" <> sessionName cwd

sessionName :: String -> Text
sessionName cwd = case Text.uncons $ Text.replace "/" "-" $ from cwd of
  Just ('-', rest) -> rest
  _ -> error $ "Current working directory should begin with /"
