module Mah.Agents.Data.Config where

import Data.Text (Text)
import Mah.Agents.Data.Temperature (Temperature)
import Mah.Agents.Pretty (Pretty(..))

data Config = Config
  { temperature :: Temperature
  , url :: Text
  }

instance Pretty Config where
  pretty Config{..} =
       "\n  temperature: " <> pretty temperature
    <> "\n  url: " <> url
