module Agent.Bob.Data.Message where

import Agent.Bob.Prelude
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Agent.Bob.Data.Role (Role)
import Agent.Bob.Data.Thinking (Thinking(..))

-- | An OpenAI-spec message from/to an agent.
data Message = Message
  { content :: Text
  , role :: Role
  , thinking :: Maybe Thinking
  } deriving Show

-- | Drop the thinking, e.g. before replaying or persisting the message.
withoutThinking :: Message -> Message
withoutThinking Message{..} = Message{thinking=Nothing, ..}

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" \o -> do
    content <- o .: "content"
    role <- o .: "role"
    thinking <- o .:? "reasoning_content"
    pure Message{..}

instance Aeson.ToJSON Message where
  toJSON Message{..} = Aeson.object $ catMaybes
    [ Just ("content", Aeson.toJSON content)
    , thinking <&> \t -> ("reasoning_content", Aeson.toJSON t)
    , Just ("role", Aeson.toJSON role)
    ]
