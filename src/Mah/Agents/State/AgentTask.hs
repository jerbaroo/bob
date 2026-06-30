module Mah.Agents.State.AgentTask where

import Data.String (IsString(..))
import Data.Text (Text)
import Witch (From(..), via)

newtype AgentTask = AgentTask Text
  deriving (From Text)

instance IsString AgentTask where
  fromString = via @Text

instance From AgentTask Text
