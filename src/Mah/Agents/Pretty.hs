module Mah.Agents.Pretty where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)

class Pretty a where
  pretty :: a -> Text
  default pretty :: Aeson.ToJSON a => a -> Text
  pretty = decodeUtf8 . ByteString.toStrict . Aeson.encodePretty

type Colour = Text

colour :: Colour -> Text -> Text
colour c t = c <> t <> reset

-- * Helpers

isBullet :: Text -> Bool
isBullet line = case Text.uncons line of
  Just (c, rest) ->
        isDigit c
    && (Text.isPrefixOf ". " rest || Text.isPrefixOf ".\t" rest)
  _ -> False

-- * ANSI colour codes

grey :: Colour
grey = "\ESC[90m"

cyanDim :: Colour
cyanDim = "\ESC[2;36m"

cyanBright :: Colour
cyanBright = "\ESC[1;96m"

magentaDim :: Colour
magentaDim = "\ESC[2;35m"

reset :: Text
reset = "\ESC[0m"
