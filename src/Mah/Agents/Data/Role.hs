module Mah.Agents.Data.Role where

import Data.Aeson qualified as Aeson
import Witch (from)

data Role = Assistant | System | User
  deriving stock (Eq, Show)

instance Aeson.FromJSON Role where
  parseJSON = Aeson.withText "Role" \case
    "assistant" -> pure Assistant
    "system" -> pure System
    "user" -> pure User
    x -> fail $ "Unknown Role: " <> from x

instance Aeson.ToJSON Role where
  toJSON Assistant = "assistant"
  toJSON System = "system"
  toJSON User = "user"
