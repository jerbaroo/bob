module Mah.Agents.Data.Config where

import Data.Text (Text)
import Data.Text qualified as Text
import Mah.Agents.Data.Level (Level)
import Mah.Agents.Data.Temperature (Temperature)
import Mah.Agents.Pretty (Pretty(..))

data Config = Config
  { level :: Level
  , model :: Text
  , temperature :: Temperature
  , url :: Text
  }

instance Pretty Config where
  pretty Config{..} =
    let formatItem (k, v) = "- " <> k <> ": " <> v
        items =
          [ ("log_level", pretty level)
          , ("model", model)
          , ("temperature", pretty temperature)
          , ("url", url)
          ]
    in  "Configuration:\n" <> Text.intercalate "\n" (formatItem <$> items)
