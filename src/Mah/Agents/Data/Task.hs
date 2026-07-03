module Mah.Agents.Data.Task where

import Data.Aeson qualified as Aeson
import Data.String (IsString(..))
import Data.Text (Text)
import Witch (From(..), via)

newtype Task = Task Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, From Text, Show)

instance IsString Task where
  fromString = via @Text

instance From Task Text
