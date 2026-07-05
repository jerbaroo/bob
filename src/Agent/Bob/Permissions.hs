module Agent.Bob.Permissions where

import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.Permission (Permission(..), parsePermission)
import Agent.Bob.Effect.File (File, readFile)
import Agent.Bob.Prelude hiding (readFile)
import Agent.Bob.Tool (IsTool(..), Tool(..), toolName)
import Data.Text qualified as Text

-- | The granted permissions, read fresh from the source of truth every check.
readPermissions :: (Ask Config :> es, File :> es) => Eff es [Permission]
readPermissions =
  asks (.permissionsPath)
  >>= readFile
  <&> maybe [] (mapMaybe parsePermission . Text.lines)

-- | Ensure a tool invocation is permitted before it runs; error if not.
checkPermission :: (Ask Config :> es, File :> es) => Tool -> Eff es ()
checkPermission (Tool (t :: t)) = do
  grants  <- filter ((== toolName @t) . (.toolName)) <$> readPermissions
  granted <- or <$> traverse (permissionGranted t . (.param)) grants
  unless granted
    $ error $ "No permission for tool " <> Text.unpack (from (toolName @t))
