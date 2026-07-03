module Mah.Agents.Data.Config where

import Data.Text (Text)
import Mah.Agents.Data.Level (Level)
import Mah.Agents.Data.Temperature (Temperature)
import Mah.Agents.Pretty (Pretty(..))

data Config = Config
  { level :: Level
  , model :: Text
  , session :: Text
  , temperature :: Temperature
  , timeoutSeconds :: Int
  , url :: Text
  }

instance Pretty Config where
  pretty Config{..} = "Configuration:\n" <> pretty
    [ ("log_level" :: Text, pretty level)
    , ("model_name", model)
    , ("model_temperature", pretty temperature)
    , ("model_url", url)
    , ("session_path", session)
    , ("timeout_seconds", pretty timeoutSeconds)
    ]
