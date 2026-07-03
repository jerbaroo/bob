module Mah.Agents.Data.AgentOutput where

import Control.Monad (unless)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Mah.Agents.Data.Event (IsEvent (..))
import Mah.Agents.Data.FinishReason (FinishReason (..))
import Mah.Agents.Data.Message (Message(..))
import Mah.Agents.Data.Resolution (Resolution)
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Data.Task (Task)
import Mah.Agents.Data.Thinking (Thinking(..))
import Witch (from)

-- | The final output from the agent.
data AgentOutput = AgentOutput
  { finishReason :: FinishReason
  , resolution :: Resolution
  , tasks :: [Task]
  , thinking :: Maybe Thinking
  } deriving Show

instance Aeson.FromJSON AgentOutput where
  parseJSON = Aeson.withObject "AgentOutput" $ \o -> do
    choice <- o .: "choices" >>= \case
      [choice] -> pure choice
      x -> fail $ "Expected exactly one choice, got: " <> show x
    finishReason <- choice .: "finish_reason"
    (message :: Message) <- choice .: "message"
    -- Assert role field.
    unless (message.role == Assistant) $
      fail $ "Expected assistant role, got: " <> show message.role
    OutputContent{..} <-
      case Aeson.eitherDecodeStrictText message.content of
        Left error' -> fail $ "Couldn't decode OutputContent: " <> from error'
        Right a -> pure a
    pure AgentOutput{thinking=message.thinking, ..}

instance IsEvent AgentOutput where
  eventTasks AgentOutput{..} = Just tasks

-- | The schema we told the LLM to use inside the "content" string.
data OutputContent = OutputContent
  { resolution :: Resolution
  , tasks      :: [Task]
  } deriving Show

instance Aeson.FromJSON OutputContent where
  parseJSON = Aeson.withObject "OutputContent" \o -> do
    resolution <- o .: "message"
    tasks      <- o .: "remaining_tasks"
    pure OutputContent{..}
