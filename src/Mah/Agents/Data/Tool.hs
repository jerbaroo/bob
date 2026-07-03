module Mah.Agents.Data.Tool where

import Data.Text (Text)

newtype Tool = Tool Text
  deriving Show

newtype ToolArgs = ToolArgs [Text]
  deriving Show
