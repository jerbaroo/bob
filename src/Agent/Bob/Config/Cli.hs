module Agent.Bob.Config.Cli where

import Agent.Bob.Data.Config (ConfigF(..), PartialConfig, defaultConfigPath)
import Agent.Bob.Data.Level (Level, parseLevel)
import Agent.Bob.Data.Temperature (Temperature(..))
import Agent.Bob.Prelude
import Options.Applicative ((<**>), Parser, ParserInfo, auto, eitherReader, fullDesc, header, help, helper, info, long, metavar, option, optional, progDesc, strOption)

-- | Parse command-line flags into a 'PartialConfig'.
partialConfig :: Parser PartialConfig
partialConfig = Config
  <$> optional (strOption
        ( long "config-path" <> metavar "PATH"
       <> help (   "JSON config file to read (default "
                <> from defaultConfigPath <> ")") ))
  <*> optional (strOption
        ( long "engine-url" <> metavar "URL"
       <> help "Chat-completions endpoint" ))
  <*> optional (option (eitherReader readLevel)
        ( long "log-level" <> metavar "LEVEL"
       <> help "One of: debug, info, meta, thinking" ))
  <*> optional (strOption
        ( long "memory-path" <> metavar "PATH"
       <> help "Session event log to read and append to" ))
  <*> optional (strOption
        ( long "model-name" <> metavar "NAME"
       <> help "Model to call" ))
  <*> optional (Temperature <$> option auto
        ( long "model-temp" <> metavar "FLOAT"
       <> help "Sampling temperature" ))
  <*> optional (strOption
        ( long "permissions-path" <> metavar "PATH"
       <> help "Granted-permissions file" ))
  -- 'projectRoot' is derived from the git root at startup, not settable.
  <*> pure Nothing
  -- 'soulPath' is optional in a resolved config, so a partial config nests it:
  -- @Just (Just p)@ = set this soul, @Nothing@ = unspecified.
  <*> optional (Just <$> strOption
        ( long "soul-path" <> metavar "PATH"
       <> help "Persona file injected into the prompt" ))
  <*> optional (option auto
        ( long "timeout-seconds" <> metavar "N"
       <> help "Model timeout in seconds" ))

-- | The full parser, includes a @--help@ command.
parserInfo :: ParserInfo PartialConfig
parserInfo = info (partialConfig <**> helper)
  ( fullDesc
 <> header "agent-bob - a minimal agent harness"
 <> progDesc "Flags override the config file, which overrides the defaults." )

-- | 'parseLevel' adapted to optparse's 'String'-based reader.
readLevel :: String -> Either String Level
readLevel = either (Left . from) Right . parseLevel . from
