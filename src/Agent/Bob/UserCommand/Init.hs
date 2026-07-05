module Agent.Bob.UserCommand.Init where

import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Effect.File (readFile, writeFile)
import Agent.Bob.Prelude hiding (readFile, writeFile)
import Agent.Bob.Pretty (pretty)
import Agent.Bob.Soul (defaultSoul)
import Agent.Bob.UserCommand (IsUserCommand(..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

-- | Create default versions of any missing configuration files: @/init@.
data Init = Init

instance Aeson.ToJSON Init where
  toJSON Init = "Init"

instance IsCommand Init where
  type Result Init = ()
  runCommand _agentId Init = do
    config :: Config <- ask
    let files =
             [ (config.configPath, configTemplate config)
             , (config.permissionsPath, defaultPermissions)
             ]
          <> maybe [] (\path -> [(path, defaultSoul)]) config.soulPath
    reports <- forM files \(path, contents) ->
      readFile path >>= \case
        Just _  -> pure $ "Kept " <> path <> " (already exists)."
        Nothing -> do
          writeFile path contents
          pure $ "Created " <> path <> "."
    writeSection System Info $ Text.intercalate "\n" reports
    pure ((), [])

instance IsEvent Init where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand Init where
  mkUserCommand [] = Just Init
  mkUserCommand _ = Nothing
  userCommandDescription =
    "Create default versions of any missing configuration files."
  userCommandName = ["/init"]

-- | A project config template.
configTemplate :: Config -> Text
configTemplate config = (<> "\n") $ pretty $ Aeson.object
  [ ("engine_url", Aeson.toJSON config.engineUrl)
  , ("log_level", Aeson.String $ from config.logLevel)
  , ("model_name", Aeson.toJSON config.modelName)
  , ("model_temperature", Aeson.toJSON config.modelTemperature)
  , ("timeout_seconds", Aeson.toJSON config.timeoutSeconds)
  ]

-- | A permissions file documenting the format, granting nothing.
defaultPermissions :: Text
defaultPermissions = Text.unlines
  [ "# One permission per line: <ToolName> <param>."
  , "# Blank lines and lines starting with # are ignored."
  , "# For example, to let the ReadFiles tool read anything under src/:"
  , "#"
  , "# ReadFiles src/**"
  ]
