module Mah.Agents.Data.Message where

import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | An OpenAI-spec message to an agent.
newtype Message = Message (Map Text Text)

instance Aeson.ToJSON Message where
  toJSON (Message m) = Aeson.toJSON m

toMessage :: Text -> Text -> Message
toMessage role content =
  Message $ Map.fromList [("role", role), ("content", content)]
