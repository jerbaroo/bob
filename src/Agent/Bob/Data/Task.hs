module Agent.Bob.Data.Task where

import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.Prompt (ToPrompt(..))

newtype Task = Task Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, From Text, IsString, Show)

instance From Task Text

instance Pretty Task where
  pretty (Task text) = text

instance ToPrompt Task where
  toPrompt = into @Text
