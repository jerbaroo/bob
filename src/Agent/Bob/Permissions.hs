module Agent.Bob.Permissions where

import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.HarnessState (HarnessState)
import Agent.Bob.Data.Permission (Permission(..), parsePermission)
import Agent.Bob.Effect.Console (Console)
import Agent.Bob.Effect.File (File, readFileLogErr)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.Tool (IsTool(..), Tool(..), toolName)
import Data.Text qualified as Text

-- | The granted permissions, read fresh from the source of truth every check.
readPermissions
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => Eff es [Permission]
readPermissions =
  asks (.permissionsPath)
  >>= readFileLogErr
  <&> maybe [] (mapMaybe parsePermission . Text.lines)

-- | Render the granted permissions for the console; shared by the startup
-- report and @/permissions show@.
renderPermissions :: [Permission] -> Text
renderPermissions [] = "No permissions granted."
renderPermissions permissions =
  "Permissions:\n" <> pretty (((.toolName) &&& (.param)) <$> permissions)

-- | Check whether a tool invocation is permitted.
checkPermission
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => Tool -> Eff es Bool
checkPermission (Tool (t :: t)) = do
  grants  <- filter ((== toolName @t) . (.toolName)) <$> readPermissions
  or <$> traverse (permissionGranted t . (.param)) grants
