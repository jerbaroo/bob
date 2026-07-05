module Agent.Bob.UserCommand.PermissionsShow where

import Data.Aeson qualified as Aeson
import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Permission (Permission(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Permissions (readPermissions)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.UserCommand (IsUserCommand(..))

-- | Print the granted permissions to the console: @/permissions show@.
data PermissionsShow = PermissionsShow

instance Aeson.ToJSON PermissionsShow where
  toJSON PermissionsShow = "PermissionsShow"

instance IsCommand PermissionsShow where
  type Result PermissionsShow = ()
  runCommand _agentId PermissionsShow = do
    permissions <- readPermissions
    writeSection System Info $ case permissions of
      [] -> "No permissions granted."
      _  -> "Permissions:\n"
        <> pretty (((.toolName) &&& (.param)) <$> permissions)
    pure ((), [])

instance IsEvent PermissionsShow where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand PermissionsShow where
  mkUserCommand [] = Just PermissionsShow
  mkUserCommand _ = Nothing
  userCommandDescription = "Print the granted permissions."
  userCommandName = ["/permissions", "show"]
