module Mah.Agents.Data.Thinking where

import Data.Aeson qualified as Aeson
import Data.Char (isDigit)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Mah.Agents.Pretty (Pretty(..))
import Mah.Agents.Pretty qualified as Pretty
import Witch (From(..), into, via)

newtype Thinking = Thinking Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, From Text, Show)

instance From Thinking Text

instance From String Thinking where
  from = via @Text

instance IsString Thinking where
  fromString = from

-- | Render thoughts as dimmed, lightly markdown-aware text.
instance Pretty Thinking where
  pretty = Text.intercalate "\n" . fmap styleLine . Text.lines . Text.strip . into @Text
   where
    styleLine :: Text -> Text
    styleLine line =
      let (indent, trimmed) = Text.span (== ' ') line
      in case bulletMarker trimmed of
           Just (marker, rest) ->
                indent
             <> Pretty.colour Pretty.cyanDim marker
             <> Pretty.colour Pretty.grey rest
           Nothing -> Pretty.colour (lineColour trimmed) line
    lineColour trimmed
      | Text.isPrefixOf "```" trimmed = Pretty.magentaDim
      | Text.isPrefixOf "#"   trimmed = Pretty.cyanBright
      | otherwise                     = Pretty.grey

-- | Split a leading markdown bullet marker ("-", "*", "+", or a numbered
-- "N." / "N)") followed by a space, from the rest of the line.
bulletMarker :: Text -> Maybe (Text, Text)
bulletMarker t
  -- A "-", "*" or "+", followed by one space.
  | Just (c, rest) <- Text.uncons t
    , c `elem` ['-', '*', '+']
    , Text.isPrefixOf " " rest
    = Just (Text.singleton c, rest)
  -- One or more digits, then a "." or ")", followed by one space.
  | (digits, rest) <- Text.span isDigit t
    , not (Text.null digits)
    , Just (x, rest') <- Text.uncons rest
    , x `elem` ['.', ')']
    , Text.isPrefixOf " " rest'
    = Just (digits <> Text.singleton x, rest')
  | otherwise = Nothing
