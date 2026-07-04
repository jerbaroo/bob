module Mah.Agents.Data.ToolName where

import Data.Aeson qualified as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Mah.Agents.Pretty (Pretty)
import Witch (From(..), via)

-- | The name the model uses to request a tool.
newtype ToolName = ToolName Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, From Text, IsString, Pretty, Show)

instance From ToolName Text where
  from (ToolName text) = text

instance From ToolName String where
  from = via @Text
