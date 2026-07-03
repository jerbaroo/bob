module Mah.Agents.Data.Message where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Mah.Agents.Data.Role (Role)
import Mah.Agents.Data.Thinking (Thinking(..))

-- | An OpenAI-spec message from/to an agent.
data Message = Message
  { content :: Text
  , role :: Role
  , thinking :: Maybe Thinking
  }

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" \o -> do
    content <- o .: "content"
    role <- o .: "role"
    thinking <- o .: "reasoning_content"
    pure Message{..}

instance Aeson.ToJSON Message where
  toJSON Message{..} = Aeson.object $ catMaybes
    [ Just ("content", Aeson.toJSON content)
    , thinking <&> \t -> ("reasoning_content", Aeson.toJSON t)
    , Just ("role", Aeson.toJSON role)
    ]
