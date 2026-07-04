module Mah.Agents.Pretty where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Witch (from)

class Pretty a where
  pretty :: a -> Text
  default pretty :: Aeson.ToJSON a => a -> Text
  pretty = decodeUtf8 . ByteString.toStrict . Aeson.encodePretty

-- | Compact, single-line JSON encoding. Suitable for one-value-per-line
-- (NDJSON) output, unlike 'pretty' which indents across multiple lines.
compact :: Aeson.ToJSON a => a -> Text
compact = decodeUtf8 . from . Aeson.encode

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty = Text.intercalate "\n" . fmap (("- " <>) . pretty)

instance (Pretty a, Pretty b) => Pretty [(a, b)] where
  pretty items =
    let maxLength = maximum $ Text.length . pretty . fst <$> items
    in  pretty $ items <&> \(k, v) ->
             pretty k
          <> Text.replicate (maxLength - Text.length (pretty k)) " "
          <> "    "
          <> pretty v

instance Pretty Aeson.Object where
  pretty = pretty . Aeson.Object

instance Pretty Aeson.Value

instance Pretty Float where
  pretty = from . show

instance Pretty Int where
  pretty = from . show

instance Pretty Text where
  pretty = id

-- * ANSI colour codes

type Colour = Text

colour :: Colour -> Text -> Text
colour c t = c <> t <> reset

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
