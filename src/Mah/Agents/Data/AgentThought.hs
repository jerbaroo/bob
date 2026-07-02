module Mah.Agents.Data.AgentThought where

import Data.String (IsString(..))
import Data.Text (Text)
import Witch (From(..), via)

newtype AgentThought = AgentThought Text
  deriving newtype (From Text, Show)

instance From AgentThought Text

instance From String AgentThought where
  from = via @Text

instance IsString AgentThought where
  fromString = from
