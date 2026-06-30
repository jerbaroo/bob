module Mah.Agents.Config where

import Data.Text (Text)
import Mah.Agents.Pretty (Pretty(..))

data Config = Config
  { model :: Text
  , runner :: Text
  }

instance Pretty Config where
  pretty Config{..} =
       "\n  model: " <> model
    <> "\n  runner: " <> runner
