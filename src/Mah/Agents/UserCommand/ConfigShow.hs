module Mah.Agents.UserCommand.ConfigShow where

import Data.Aeson qualified as Aeson
import Data.Effect.Reader (ask)
import Data.Text qualified as Text
import Mah.Agents.Command (IsCommand(..))
import Mah.Agents.Data.Config (Config)
import Mah.Agents.Data.Event (IsEvent(..))
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Effect.Console (writeSection)
import Mah.Agents.Pretty (Pretty(..))
import Mah.Agents.UserCommand (IsUserCommand(..))
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
