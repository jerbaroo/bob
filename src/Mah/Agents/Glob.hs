module Mah.Agents.Glob where

import Data.String (IsString)
import Data.Text (Text)
import System.FilePath.Glob qualified as Glob
import Witch (from)

-- | A glob pattern.
newtype Glob = Glob Text
  deriving newtype (Eq, IsString, Show)

-- | Does the path match the glob?
matches :: Glob -> Text -> Bool
matches (Glob g) = Glob.match (Glob.compile $ from g) . from
