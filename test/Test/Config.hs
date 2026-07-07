module Test.Config (spec) where

import Agent.Bob.Config.Cli qualified as Cli
import Agent.Bob.Config.File qualified as File
import Agent.Bob.Data.Config (Config, ConfigF(..), PartialConfig, applyPartial)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Prelude
import Agent.Bob.Pretty (pretty)
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Options.Applicative (defaultPrefs, execParserPure, getParseResult)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | A base (default) config for exercising 'applyPartial'; only the fields the
-- tests inspect matter.
baseConfig :: Config
baseConfig = Config
  { configPath = "default.json"
  , engineUrl = "default-engine"
  , gitBranch = "default-branch"
  , logLevel = Debug
  , memoryPath = "default-memory"
  , modelName = "DefaultModel"
  , modelTemperature = 0.5
  , noAsk = False
  , permissionsPath = "default-perms"
  , projectRoot = "default-root"
  , skillsPaths = []
  , soulPath = Nothing
  , timeoutSeconds = 60
  }

-- | Run the CLI parser purely, as 'main' would over @argv@.
parseCli :: [String] -> Maybe PartialConfig
parseCli = getParseResult . execParserPure defaultPrefs Cli.parserInfo

-- | 'parseCli', failing the test on a parse error.
cliPartial :: [String] -> IO PartialConfig
cliPartial = maybe (fail "CLI parse failed") pure . parseCli

-- | 'File.parseConfig', failing the test on a parse error.
filePartial :: Text -> IO PartialConfig
filePartial = either (fail . from) pure . File.parseConfig

spec :: Spec
spec = describe "config parsers" $ do
  it "CLI flags override the default config" $ do
    partial <- cliPartial
      ["--model-name", "Cli", "--log-level", "info", "--timeout-seconds", "5"]
    let config = applyPartial partial baseConfig
    (config.modelName, pretty config.logLevel, config.timeoutSeconds)
      `shouldBe` ("Cli", "info", 5)

  it "supports the --flag=value spelling" $ do
    partial <- cliPartial ["--engine-url=http://x"]
    (applyPartial partial baseConfig).engineUrl `shouldBe` "http://x"

  it "rejects unknown flags" $
    isNothing (parseCli ["--nope", "x"]) `shouldBe` True

  it "rejects unparseable values" $
    isNothing (parseCli ["--timeout-seconds", "abc"]) `shouldBe` True

  it "a file config overrides the default, keeping unspecified fields" $ do
    partial <- filePartial "{\"model_name\": \"File\"}"
    let config = applyPartial partial baseConfig
    (config.modelName, config.engineUrl)
      `shouldBe` ("File", "default-engine")

  it "an empty JSON object leaves everything at its default" $ do
    partial <- filePartial "{}"
    (applyPartial partial baseConfig).modelName `shouldBe` "DefaultModel"

  it "rejects config_path in the config file" $
    isLeft (File.parseConfig "{\"config_path\": \"other.json\"}") `shouldBe` True

  it "rejects project_root in the config file" $
    isLeft (File.parseConfig "{\"project_root\": \"/elsewhere\"}") `shouldBe` True

  it "rejects git_branch in the config file" $
    isLeft (File.parseConfig "{\"git_branch\": \"other\"}") `shouldBe` True

  it "rejects an unknown log level in the config file" $
    isLeft (File.parseConfig "{\"log_level\": \"nope\"}") `shouldBe` True

  it "precedence is CLI > project file > global file > default" $ do
    cli <- cliPartial ["--model-name", "Cli"]
    project <- filePartial
      "{\"model_name\": \"Project\", \"engine_url\": \"project-engine\"}"
    global <- filePartial $ "{\"model_name\": \"Global\","
      <> "\"engine_url\": \"global-engine\", \"memory_path\": \"global-mem\"}"
    let config = applyPartial (cli <> project <> global) baseConfig
    -- CLI wins for model_name; the project file beats the global file for
    -- engine_url; the global file fills memory_path; default keeps the rest.
    (config.modelName, config.engineUrl, config.memoryPath, config.permissionsPath)
      `shouldBe` ("Cli", "project-engine", "global-mem", "default-perms")
