module Mah.Agents.Data.ToolName where

import Control.Monad.Hefty (Eff, (:>))
import Data.Aeson qualified as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Mah.Agents.Effect.File (File)
import Witch (From(..), via)

-- | The name the model uses to request a tool.
newtype ToolName = ToolName Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, From Text, IsString, Show)

instance From ToolName Text where
  from (ToolName text) = text

instance From ToolName String where
  from = via @Text
