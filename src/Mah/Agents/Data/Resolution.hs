module Mah.Agents.Data.Resolution where

import Data.Aeson qualified as Aeson
import Data.String (IsString(..))
import Data.Text (Text)
import Mah.Agents.Pretty (Pretty(..))
import Witch (From(..), via)

newtype Resolution = Resolution Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, From Text, Show)

instance From String Resolution where
  from = via @Text

instance IsString Resolution where
  fromString = from

instance Pretty Resolution where
  pretty (Resolution text) = text
