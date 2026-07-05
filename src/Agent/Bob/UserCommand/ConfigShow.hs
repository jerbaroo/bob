module Agent.Bob.UserCommand.ConfigShow where

import Data.Aeson qualified as Aeson
import Data.Effect.Reader (ask)
import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Config (Config)
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.UserCommand (IsUserCommand(..))

-- | Print the current configuration to the console: @/config show@.
data ConfigShow = ConfigShow

instance Aeson.ToJSON ConfigShow where
  toJSON ConfigShow = "ConfigShow"

instance IsCommand ConfigShow where
  type Result ConfigShow = ()
  runCommand _agentId ConfigShow = do
    config :: Config <- ask
    writeSection System Info $ pretty config
    pure ((), [])

instance IsEvent ConfigShow where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand ConfigShow where
  mkUserCommand [] = Just ConfigShow
  mkUserCommand _ = Nothing
  userCommandDescription = "Print the current configuration."
  userCommandName = ["/config", "show"]
