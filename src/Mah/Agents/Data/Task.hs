module Mah.Agents.Data.Task where

import Data.Aeson qualified as Aeson
import Data.String (IsString(..))
import Data.Text (Text)
import Mah.Agents.Pretty (Pretty(..))
import Mah.Agents.Prompt (ToPrompt(..))
import Witch (From(..), into, via)

newtype Task = Task Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, From Text, Show)

instance IsString Task where
  fromString = via @Text

instance From Task Text

instance Pretty Task where
  pretty (Task text) = text

instance ToPrompt Task where
  toPrompt = into @Text
