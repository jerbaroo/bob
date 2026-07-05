module Agent.Bob.Glob where

import Agent.Bob.Prelude
import System.FilePath.Glob qualified as Glob

-- | A glob pattern.
newtype Glob = Glob Text
  deriving newtype (Eq, IsString, Show)

-- | Does the path match the glob?
matches :: Glob -> Text -> Bool
matches (Glob g) = Glob.match (Glob.compile $ from g) . from
