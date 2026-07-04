module Mah.Agents.UserCommand.PermissionsShow where

import Control.Arrow ((&&&))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Mah.Agents.Command (IsCommand(..))
import Mah.Agents.Data.Event (IsEvent(..))
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Permission (Permission(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Effect.Console (writeSection)
import Mah.Agents.Permissions (readPermissions)
import Mah.Agents.Pretty (Pretty(..))
import Mah.Agents.UserCommand (IsUserCommand(..))
import Witch (from)

-- | Print the granted permissions to the console: @/permissions show@.
data PermissionsShow = PermissionsShow

instance IsUserCommand PermissionsShow where
  parseUserCommand input
    | Text.words (from input) == ["/permissions", "show"] = Just PermissionsShow
    | otherwise = Nothing

instance IsCommand PermissionsShow where
  type Result PermissionsShow = ()
  runCommand _agentId PermissionsShow = do
    permissions <- readPermissions
    writeSection System Info $ case permissions of
      [] -> "No permissions granted."
      _  -> "Permissions:\n"
        <> pretty (((.toolName) &&& (.param)) <$> permissions)
    pure ((), [])

instance Aeson.ToJSON PermissionsShow where
  toJSON PermissionsShow = Aeson.String "PermissionsShow"

instance IsEvent PermissionsShow where
  eventName _ = "PermissionsShow"
