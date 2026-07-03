module Mah.Agents.Data.Config where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Mah.Agents.Data.Level (Level)
import Mah.Agents.Data.Temperature (Temperature)
import Mah.Agents.Pretty (Pretty(..))

data Config = Config
  { engineUrl :: Text
  , logLevel :: Level
  , modelName :: Text
  , memoryPath :: Text
  , modelTemperature :: Temperature
  , soulPath :: Maybe Text
  , timeoutSeconds :: Int
  }

instance Pretty Config where
  pretty Config{..} =
    let prettyConfig = pretty $ catMaybes
          [ Just ("engine_url", engineUrl)
          , Just ("log_level" :: Text, pretty logLevel)
          , Just ("model_name", modelName)
          , Just ("memory_path", memoryPath)
          , Just ("model_temperature", pretty modelTemperature)
          , ("soul_path",) <$> soulPath
          , Just ("timeout_seconds", pretty timeoutSeconds)
          ]
    in "Configuration:\n" <> prettyConfig
