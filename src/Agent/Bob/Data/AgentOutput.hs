module Agent.Bob.Data.AgentOutput where

import Control.Monad (unless)
import Data.Aeson ((.:), (.:?), (.!=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Agent.Bob.Data.Event (IsEvent (..))
import Agent.Bob.Data.FinishReason (FinishReason (..))
import Agent.Bob.Data.Message (Message(..), withoutThinking)
import Agent.Bob.Data.Resolution (Resolution)
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.Task (Task)
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.Tool (Tool)
import Agent.Bob.Tool.Registry (parseToolRequest)
import Witch (From(..))

-- | The final output from the agent.
--
-- Everything besides 'raw' is parsed from it once, up front, so failures
-- surface at the boundary; use @From AgentOutput X@ instances for variations.
data AgentOutput = AgentOutput
  { agentOutputMeta :: AgentOutputMeta
  , agentMessageContent :: AgentMessageContent
  , message :: Message
  -- ^ The assistant message with "content" unparsed.
  -- Use 'agentMessageContent' for the parsed "content".
  , raw :: Aeson.Object
  -- ^ The full unparsed JSON from the model.
  } deriving Show

instance Aeson.FromJSON AgentOutput where
  parseJSON = Aeson.withObject "AgentOutput" $ \o -> do
    (choice, message) <- o .: "choices" >>= \case
      [choice] -> (choice,) <$> (choice .: "message")
      x -> fail $ "Expected exactly one choice, got: " <> show x
    -- Assert role field.
    unless (message.role == Assistant) $
      fail $ "Expected assistant role, got: " <> show message.role
    agentMessageContent <-
      case Aeson.eitherDecodeStrictText message.content of
        Left error' -> fail $ "Couldn't decode AgentMessageContent: " <> from error'
        Right a -> pure a
    finishReason <- choice .: "finish_reason"
    timings <- o .: "timings"
    timePredicted <- timings .: "predicted_ms"
    timePrompt <- timings .: "prompt_ms"
    usage <- o .: "usage"
    tokensCached <- (usage .: "prompt_tokens_details") >>= (.: "cached_tokens")
    tokensTotal <- usage .: "total_tokens"
    pure AgentOutput
      { agentOutputMeta=AgentOutputMeta{..}
      , raw=o
      , ..
      }

-- | The raw engine response, unchanged, so the event round-trips exactly.
instance Aeson.ToJSON AgentOutput where
  toJSON AgentOutput{raw} = Aeson.Object raw

-- | Everything is derived from 'raw'.
instance Eq AgentOutput where
  a == b = a.raw == b.raw

instance IsEvent AgentOutput where
  eventTasks AgentOutput{..} = Just agentMessageContent.tasks
  -- Thinking is stripped: replaying every past turn's chain-of-thought
  -- would bloat the context without informing the model.
  eventToMessage AgentOutput{..} = Just $ withoutThinking message

instance Pretty AgentOutput where
  pretty AgentOutput{} = "AgentOutput{..}" -- Too verbose to print anymore.

-- | The schema we told the LLM to use inside the "content" string.
--
-- This exists just as a helper for parsing 'AgentOutput'.
data AgentMessageContent = AgentMessageContent
  { resolution :: Resolution
  , tasks      :: [Task]
  , toolRequests :: [Tool]
  } deriving Show

instance Aeson.FromJSON AgentMessageContent where
  parseJSON = Aeson.withObject "AgentMessageContent" \o -> do
    resolution   <- o .: "message"
    tasks        <- o .: "remaining_tasks"
    toolCalls    <- o .:? "tool_calls" .!= []
    toolRequests <- traverse parseToolCall toolCalls
    pure AgentMessageContent{..}
   where
    parseToolCall = Aeson.withObject "tool_call" \c -> do
      name <- c .: "name"
      args <- c .:? "arguments" .!= Aeson.Object mempty
      parseToolRequest name args

-- | Separate type for pretty printing meta-level information.
data AgentOutputMeta = AgentOutputMeta
  { finishReason :: FinishReason
  , timePredicted :: Float
  , timePrompt :: Float
  , tokensCached :: Int
  , tokensTotal :: Int
  } deriving Show

instance From AgentOutput AgentOutputMeta where
  from AgentOutput{..} = agentOutputMeta

instance Pretty AgentOutputMeta where
  pretty AgentOutputMeta{..} = pretty
    [ ("finish_reason" :: Text, pretty finishReason)
    , ("time_predicted", pretty timePredicted)
    , ("time_prompt", pretty timePrompt)
    , ("token_cached", pretty tokensCached)
    , ("token_total", pretty tokensTotal)
    ]
