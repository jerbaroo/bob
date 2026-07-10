module Agent.Bob.Util.Aeson where

import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Data.Text.Encoding (decodeUtf8)

-- | Encode as JSON 'Text'. 'Aeson.encode' emits compact single-line JSON
-- (no whitespace), so one value is one log line.
textViaJSON :: Aeson.ToJSON a => a -> Text
textViaJSON = decodeUtf8 . from . Aeson.encode
