module Agent.Bob.Config.File where

import Agent.Bob.Data.Config (PartialConfig)
import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Data.Text.Encoding qualified as Text

-- | Parse the contents of a JSON config file into a 'PartialConfig'. Every key
-- is optional (see the 'Aeson.FromJSON' instance); an empty object @{}@ parses
-- to the empty partial config.
parseConfig :: Text -> Either Text PartialConfig
parseConfig =
  either (Left . from) Right . Aeson.eitherDecodeStrict . Text.encodeUtf8
