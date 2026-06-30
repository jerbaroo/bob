module Mah.Agents.State.AgentThought where

import Data.String (IsString(..))
import Data.Text (Text)
import Witch (From(..), via)

newtype AgentThought = AgentThought Text
  deriving (From Text)

instance IsString AgentThought where
  fromString = via @Text

instance From AgentThought Text
