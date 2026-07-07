module Agent.Bob.Data.Config where

import Agent.Bob.Data.Level (Level)
import Agent.Bob.Data.Temperature (Temperature)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Data.Aeson ((.:?))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

-- | The machine-wide config file: the lowest-precedence file source.
globalConfigPath :: Text
globalConfigPath = "~/.config/agent-bob/config.json"

-- | Where the project config file is read from (relative to the project
-- root) unless @--config-path@ overrides it. It overrides the global file.
defaultConfigPath :: Text
defaultConfigPath = "agent-bob/config.json"

-- | Higher-kinded field wrapper.
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

-- | The harness configuration.
data ConfigF f = Config
  { configPath :: HKD f Text
  , engineUrl :: HKD f Text
  , gitBranch :: HKD f Text
  -- ^ Derived from @.git/HEAD@ at startup (sanitised branch, short SHA when
  -- detached, or @default@ outside a repo), not user-configurable.
  , logLevel :: HKD f Level
  , memoryPath :: HKD f Text
  , modelName :: HKD f Text
  , modelTemperature :: HKD f Temperature
  , noAsk :: HKD f Bool
  -- ^ Deny unpermitted tool calls outright instead of prompting the user.
  , permissionsPath :: HKD f Text
  , projectRoot :: HKD f Text
  -- ^ Derived from the git root (or CWD) at startup, not user-configurable.
  , skillsPaths :: HKD f [Text]
  -- ^ Directories scanned for skills at startup (each holds @<skill>/SKILL.md@).
  , soulPath :: HKD f (Maybe Text)
  , timeoutSeconds :: HKD f Int
  }

-- | A fully-resolved configuration.
type Config = ConfigF Identity

-- | A set of overrides: 'Nothing' in a field means "not specified, fall back
-- to a lower-precedence source".
type PartialConfig = ConfigF Maybe

instance Pretty Config where
  pretty Config{..} =
    let prettyConfig = pretty $ catMaybes
          [ Just ("config_path", configPath)
          , Just ("engine_url", engineUrl)
          , Just ("git_branch", gitBranch)
          , Just ("log_level" :: Text, pretty logLevel)
          , Just ("memory_path", memoryPath)
          , Just ("model_name", modelName)
          , Just ("model_temperature", pretty modelTemperature)
          , Just ("no_ask", if noAsk then "true" else "false")
          , Just ("permissions_path", permissionsPath)
          , Just ("project_root", projectRoot)
          , Just ("skills_paths", Text.intercalate ", " skillsPaths)
          , ("soul_path",) <$> soulPath
          , Just ("timeout_seconds", pretty timeoutSeconds)
          ]
    in "Config:\n" <> prettyConfig

-- | The empty partial config: nothing specified.
emptyPartial :: PartialConfig
emptyPartial = Config
  { configPath = Nothing
  , engineUrl = Nothing
  , gitBranch = Nothing
  , logLevel = Nothing
  , memoryPath = Nothing
  , modelName = Nothing
  , modelTemperature = Nothing
  , noAsk = Nothing
  , permissionsPath = Nothing
  , projectRoot = Nothing
  , skillsPaths = Nothing
  , soulPath = Nothing
  , timeoutSeconds = Nothing
  }

instance Monoid PartialConfig where
  mempty = emptyPartial

-- | Left-biased: for each field the first 'Just' wins.
instance Semigroup PartialConfig where
  a <> b = Config
    { configPath = a.configPath <|> b.configPath
    , engineUrl = a.engineUrl <|> b.engineUrl
    , gitBranch = a.gitBranch <|> b.gitBranch
    , logLevel = a.logLevel <|> b.logLevel
    , memoryPath = a.memoryPath <|> b.memoryPath
    , modelName = a.modelName <|> b.modelName
    , modelTemperature = a.modelTemperature <|> b.modelTemperature
    , noAsk = a.noAsk <|> b.noAsk
    , permissionsPath = a.permissionsPath <|> b.permissionsPath
    , projectRoot = a.projectRoot <|> b.projectRoot
    -- Additive across sources (not left-biased): combine both lists.
    , skillsPaths = a.skillsPaths <> b.skillsPaths
    , soulPath = a.soulPath <|> b.soulPath
    , timeoutSeconds = a.timeoutSeconds <|> b.timeoutSeconds
    }

-- | Apply values from the partial config over a base (default) config.
applyPartial :: PartialConfig -> Config -> Config
applyPartial p c = Config
  { configPath = fromMaybe c.configPath p.configPath
  , engineUrl = fromMaybe c.engineUrl p.engineUrl
  , gitBranch = fromMaybe c.gitBranch p.gitBranch
  , logLevel = fromMaybe c.logLevel p.logLevel
  , memoryPath = fromMaybe c.memoryPath p.memoryPath
  , modelName = fromMaybe c.modelName p.modelName
  , modelTemperature = fromMaybe c.modelTemperature p.modelTemperature
  , noAsk = fromMaybe c.noAsk p.noAsk
  , permissionsPath = fromMaybe c.permissionsPath p.permissionsPath
  , projectRoot = fromMaybe c.projectRoot p.projectRoot
  -- Additive: the configured paths are added to the default, not replacing it.
  , skillsPaths = c.skillsPaths <> fromMaybe [] p.skillsPaths
  , soulPath = fromMaybe c.soulPath p.soulPath
  , timeoutSeconds = fromMaybe c.timeoutSeconds p.timeoutSeconds
  }

-- | Parse a config file's JSON into a 'PartialConfig'.
instance Aeson.FromJSON PartialConfig where
  parseJSON = Aeson.withObject "PartialConfig" \o -> do
    -- Only the CLI can choose which file is read, so a "config_path" key in
    -- the file itself doesn't make any sense.
    configPath <- o .:? "config_path" >>= \case
      Nothing -> pure Nothing
      Just (_ :: Text) -> fail "config_path should not exist in config file"
    engineUrl <- o .:? "engine_url"
    -- Derived from .git/HEAD at startup; configuring it makes no sense.
    gitBranch <- o .:? "git_branch" >>= \case
      Nothing -> pure Nothing
      Just (_ :: Text) -> fail "git_branch should not exist in config file"
    logLevel <- o .:? "log_level"
    memoryPath <- o .:? "memory_path"
    modelName <- o .:? "model_name"
    modelTemperature <- o .:? "model_temperature"
    noAsk <- o .:? "no_ask"
    permissionsPath <- o .:? "permissions_path"
    -- Derived from the git root at startup; configuring it makes no sense.
    projectRoot <- o .:? "project_root" >>= \case
      Nothing -> pure Nothing
      Just (_ :: Text) -> fail "project_root should not exist in config file"
    skillsPaths <- o .:? "skills_paths"
    soulPath <- o .:? "soul_path"
    timeoutSeconds <- o .:? "timeout_seconds"
    pure Config{..}
