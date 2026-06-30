module Mah.Agents.State.AgentResolution where

import Data.String (IsString(..))
import Data.Text (Text)
import Witch (From(..), via)

newtype AgentResolution = AgentResolution Text
  deriving newtype (From Text)

instance IsString AgentResolution where
  fromString = via @Text
