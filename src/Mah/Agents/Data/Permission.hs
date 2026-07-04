module Mah.Agents.Data.Permission where

import Data.Text (Text)
import Data.Text qualified as Text
import Mah.Agents.Data.ToolName (ToolName)
import Witch (from)

-- | A single granted permission: a tool and the raw parameter scoping it.
data Permission = Permission
  { toolName :: ToolName
  , param :: Text
  }

-- | Parse one line of the permissions file: @\<ToolName\> \<param\>@.
parsePermission :: Text -> Maybe Permission
parsePermission line
  | Text.null stripped                = Nothing
  | Text.isPrefixOf "#" stripped      = Nothing
  | Text.null name || Text.null param = Nothing
  | otherwise = Just $ Permission (from name) param
 where
  stripped      = Text.strip line
  (name, rest)  = Text.break (== ' ') stripped
  param         = Text.strip rest
