module Mah.Agents.Data.FinishReason where

import Data.Aeson qualified as Aeson
import Witch (from)

data FinishReason = Stop
  deriving Show

instance Aeson.FromJSON FinishReason where
  parseJSON = Aeson.withText "FinishReason" \case
    "stop" -> pure Stop
    x -> fail $ "Unknown FinishReason: " <> from x

instance Aeson.ToJSON FinishReason where
  toJSON Stop = "stop"
