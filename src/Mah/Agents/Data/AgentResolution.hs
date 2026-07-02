module Mah.Agents.Data.AgentResolution where

import Data.String (IsString(..))
import Data.Text (Text)
import Witch (From(..), via)

newtype AgentResolution = AgentResolution Text
  deriving newtype (From Text, Show)

instance From String AgentResolution where
  from = via @Text

instance IsString AgentResolution where
  fromString = from
