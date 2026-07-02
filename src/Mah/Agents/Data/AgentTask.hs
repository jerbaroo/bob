module Mah.Agents.Data.AgentTask where

import Data.String (IsString(..))
import Data.Text (Text)
import Witch (From(..), via)

newtype AgentTask = AgentTask Text
  deriving newtype (From Text, Show)

instance IsString AgentTask where
  fromString = via @Text

instance From AgentTask Text
