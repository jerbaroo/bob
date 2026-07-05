module Agent.Bob.Data.Resolution where

import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Agent.Bob.Pretty (Pretty(..))

newtype Resolution = Resolution Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, From Text, Show)

instance From String Resolution where
  from = via @Text

instance From Resolution Text

instance IsString Resolution where
  fromString = from

instance Pretty Resolution where
  pretty (Resolution text) = text
