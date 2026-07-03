module Mah.Agents.Data.Config where

import Data.Text (Text)
import Data.Text qualified as Text
import Mah.Agents.Data.Temperature (Temperature)
import Mah.Agents.Pretty (Pretty(..))

data Config = Config
  { temperature :: Temperature
  , url :: Text
  }

instance Pretty Config where
  pretty Config{..} =
    let formatItem (k, v) = "- " <> k <> ": " <> v
        items =
          [ ("temperature", pretty temperature)
          , ("url", url)
          ]
    in  "Configuration:\n" <> Text.intercalate "\n" (formatItem <$> items)
