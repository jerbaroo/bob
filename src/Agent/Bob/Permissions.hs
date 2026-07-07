module Agent.Bob.Permissions where

import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.HarnessState (HarnessState)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Permission (Permission(..), parsePermission)
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (Console, writeSection)
import Agent.Bob.Effect.File (File, readFileLogErr)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.Tool (IsTool(..), Tool(..), toolName)
import Data.Either (partitionEithers)
import Data.Text qualified as Text

-- | Is a tool invocation is permitted?
checkPermission
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => Tool -> Eff es Bool
checkPermission (Tool (t :: t)) = do
  grants  <- filter ((== toolName @t) . (.toolName)) <$> readPermissions
  or <$> traverse (permissionGranted t . (.param)) grants

-- | The granted permissions, read fresh from the source of truth.
-- Malformed lines are written to the console as errors.
readPermissions
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => Eff es [Permission]
readPermissions = do
  lines' <- asks (.permissionsPath) >>= readFileLogErr <&> maybe [] Text.lines
  let (malformed, grants) = partitionEithers $ mapMaybe parsePermission lines'
  forM_ malformed $ writeSection System Error . ("Malformed permission line: " <>)
  pure grants

-- | Render the granted permissions for the console.
renderPermissions :: [Permission] -> Text
renderPermissions [] = "No permissions granted."
renderPermissions permissions =
  "Permissions:\n" <> pretty (((.toolName) &&& (.param)) <$> permissions)
