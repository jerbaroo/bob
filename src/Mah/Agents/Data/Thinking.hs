module Mah.Agents.Data.Thinking where

import Data.Aeson qualified as Aeson
import Data.List (intercalate)
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

instance Pretty Thinking where
  pretty thinking =
    Text.unlines
    $ intercalate [""] -- Unlines will insert a newline between groups of thoughts.
    $ fmap applyColour <$> toThoughts thinking

-- * Format thoughts

type Thought = Text

applyColour :: Thought -> Thought
applyColour line' =
  let line = Text.strip line'
      colour | Pretty.isBullet line = Pretty.grey
             | Text.isPrefixOf "```" line = Pretty.magentaDim
             | otherwise = Pretty.grey
  in  Pretty.colour colour line'

chunkThoughts :: [Thought] -> [[Thought]]
chunkThoughts =
    filter (not . null) -- Remove empty groups of thoughts.
  . fmap (filter $ not . Text.null) -- Remove empty thoughts.
  . chunkThoughts'
 where
  chunkThoughts' :: [Thought] -> [[Thought]]
  chunkThoughts' [] = []
  chunkThoughts' (x:xs) =
    let (chunk, rest) = break (\t -> Text.null t || Pretty.isBullet t) xs
    in  (x : chunk) : chunkThoughts rest

toThoughts :: Thinking -> [[Thought]]
toThoughts =
    chunkThoughts -- Chunk into groups of thoughts.
  . fmap Text.stripEnd -- Remove trailing whitespace.
  . Text.lines -- Convert into lines of thought.
  . Text.strip -- Strip surrounding whitespace.
  . into @Text
