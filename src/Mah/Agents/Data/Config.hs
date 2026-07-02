module Mah.Agents.Data.Config where

import Data.Text (Text)
import Mah.Agents.Data.Temperature (Temperature)
import Mah.Agents.Pretty (Pretty(..))

data Config = Config
  { model :: Text
  , runner :: Text
  , temperature :: Temperature
  }

instance Pretty Config where
  pretty Config{..} =
       "\n  model: " <> model
    <> "\n  runner: " <> runner
