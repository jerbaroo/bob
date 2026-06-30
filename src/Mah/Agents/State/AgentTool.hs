module Mah.Agents.State.AgentTool where

import Data.Text (Text)

newtype AgentTool = AgentTool Text
  deriving Show

newtype AgentToolArgs = AgentToolArgs [Text]
  deriving Show
