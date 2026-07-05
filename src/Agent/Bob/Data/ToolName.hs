module Agent.Bob.Data.ToolName where

import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Data.Typeable (TypeRep)
import Agent.Bob.Pretty (Pretty)

-- | The name the model uses to request a tool.
newtype ToolName = ToolName Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Eq, From Text, IsString, Pretty, Show)

instance From ToolName Text where
  from (ToolName text) = text

instance From ToolName String where
  from = via @Text

instance From TypeRep ToolName where
  from = via @Text . show
