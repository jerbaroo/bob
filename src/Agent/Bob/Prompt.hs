module Agent.Bob.Prompt where

import Data.Text (Text)
import Data.Text qualified as Text

-- | Render a value as plain text destined for the model's prompt.
class ToPrompt a where
  toPrompt :: a -> Text

-- | Render a list as a bulleted block, one item per line.
instance ToPrompt a => ToPrompt [a] where
  toPrompt = Text.intercalate "\n" . fmap (("- " <>) . toPrompt)
