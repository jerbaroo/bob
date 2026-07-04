module Mah.Agents.Permissions where

import Control.Monad (unless)
import Control.Monad.Hefty (Eff, (:>))
import Data.Effect.Reader (Ask, asks)
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Prelude hiding (readFile)
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.Permission (Permission(..), parsePermission)
import Mah.Agents.Effect.File (File, readFile)
import Mah.Agents.Tool (IsTool(..), Tool(..), toolName)
import Witch (from)

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
