module Agent.Bob.Data.FinishReason where

import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Agent.Bob.Pretty (Pretty(..))

data FinishReason = Stop
  deriving Show

instance Aeson.FromJSON FinishReason where
  parseJSON = Aeson.withText "FinishReason" \case
    "stop" -> pure Stop
    x -> fail $ "Unknown FinishReason: " <> from x

instance Aeson.ToJSON FinishReason where
  toJSON Stop = "stop"

instance Pretty FinishReason where
  pretty Stop = "stop"
