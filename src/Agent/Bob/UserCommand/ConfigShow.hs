module Agent.Bob.UserCommand.ConfigShow where

import Data.Aeson qualified as Aeson
import Data.Effect.Reader (ask)
import Data.Text qualified as Text
import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Config (Config)
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.UserCommand (IsUserCommand(..))
import Witch (from)

-- | Print the current configuration to the console: @/config show@.
data ConfigShow = ConfigShow

instance IsUserCommand ConfigShow where
  parseUserCommand input
    | Text.words (from input) == ["/config", "show"] = Just ConfigShow
    | otherwise = Nothing

instance IsCommand ConfigShow where
  type Result ConfigShow = ()
  runCommand _agentId ConfigShow = do
    config :: Config <- ask
    writeSection System Info $ pretty config
    pure ((), [])

instance Aeson.ToJSON ConfigShow where
  toJSON ConfigShow = Aeson.String "ConfigShow"

instance IsEvent ConfigShow where
  eventName _ = "ConfigShow"
