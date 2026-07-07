module Agent.Bob.Data.Permission where

import Agent.Bob.Prelude
import Data.Text qualified as Text
import Agent.Bob.Data.ToolName (ToolName)

-- | The user's answer to a permission request.
data Decision = AllowOnce | AllowAlways | Deny
  deriving (Eq, Show)

parseDecision :: Text -> Decision
parseDecision answer = case Text.toLower $ Text.strip answer of
  "y" -> AllowOnce
  "a" -> AllowAlways
  _   -> Deny

-- | A single granted permission: a tool and the raw parameter scoping it.
data Permission = Permission
  { toolName :: ToolName
  , param :: Text
  }

-- | Parse one line of the permissions file: @\<ToolName\> \<param\>@.
-- 'Nothing' for blank/comment lines, 'Left' the raw line when malformed.
parsePermission :: Text -> Maybe (Either Text Permission)
parsePermission line
  | Text.null stripped                = Nothing
  | Text.isPrefixOf "#" stripped      = Nothing
  | Text.null name || Text.null param = Just $ Left stripped
  | otherwise = Just $ Right $ Permission (from name) param
 where
  stripped      = Text.strip line
  (name, rest)  = Text.break (== ' ') stripped
  param         = Text.strip rest

parsePermissionMay :: Text -> Maybe Permission
parsePermissionMay = (either (const Nothing) Just =<<) . parsePermission
