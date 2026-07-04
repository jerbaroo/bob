module Agent.Bob.Data.AgentOutput where

import Control.Monad (unless)
import Data.Aeson ((.:), (.:?), (.!=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Agent.Bob.Data.Event (IsEvent (..))
import Agent.Bob.Data.FinishReason (FinishReason (..))
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Resolution (Resolution)
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.Task (Task)
import Agent.Bob.Data.Thinking (Thinking(..))
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.Tool (Tool)
import Agent.Bob.Tool.Registry (parseToolRequest)
import Witch (From(..))
import System.IO.Unsafe (unsafePerformIO)

-- | The final output from the agent.
data AgentOutput = AgentOutput
  { agentOutputMeta :: AgentOutputMeta
  , raw :: Aeson.Object
  , resolution :: Resolution
  , tasks :: [Task]
  , thinking :: Maybe Thinking
  , toolRequests :: [Tool]
  } deriving Show

instance Aeson.FromJSON AgentOutput where
  parseJSON = Aeson.withObject "AgentOutput" $ \o' -> do
    let o = unsafePerformIO do
          putStrLn $ "Parsing AgentOutput from: " <> from (pretty o')
          pure o'
    (choice, message) <- parseChoiceAndMessage o
    -- Assert role field.
    unless (message.role == Assistant) $
      fail $ "Expected assistant role, got: " <> show message.role
    OutputContent{..} <-
      case Aeson.eitherDecodeStrictText message.content of
        Left error' -> fail $ "Couldn't decode OutputContent: " <> from error'
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
      , thinking=message.thinking
      , ..
      }

parseChoiceAndMessage :: Aeson.Object -> Aeson.Parser (Aeson.Object, Message)
parseChoiceAndMessage o = o .: "choices" >>= \case
  [choice] -> (choice,) <$> (choice .: "message")
  x -> fail $ "Expected exactly one choice, got: " <> show x

instance Aeson.ToJSON AgentOutput where
  toJSON AgentOutput{raw} = Aeson.Object raw

instance IsEvent AgentOutput where
  eventName _ = "AgentOutput"
  eventTasks AgentOutput{..} = Just tasks
  -- The assistant message exactly as the model produced it.
  eventToMessage AgentOutput{raw} =
    -- It's important we call this in @Aeson.FromJSON AgentOutput@.
    snd <$> Aeson.parseMaybe parseChoiceAndMessage raw

instance Pretty AgentOutput where
  pretty AgentOutput{} = "AgentOutput{..}" -- Too verbose to print anymore.

-- | The schema we told the LLM to use inside the "content" string.
--
-- This exists just as a helper for parsing 'AgentOutput'.
data OutputContent = OutputContent
  { resolution :: Resolution
  , tasks      :: [Task]
  , toolRequests :: [Tool]
  }

instance Aeson.FromJSON OutputContent where
  parseJSON = Aeson.withObject "OutputContent" \o -> do
    resolution   <- o .: "message"
    tasks        <- o .: "remaining_tasks"
    toolCalls    <- o .:? "tool_calls" .!= []
    toolRequests <- traverse parseToolCall toolCalls
    pure OutputContent{..}
   where
    parseToolCall = Aeson.withObject "tool_call" \c -> do
      name <- c .: "name"
      args <- c .:? "arguments" .!= Aeson.Object mempty
      parseToolRequest name args

-- | Separate type for pretty printing info-level information.
data AgentOutputInfo = AgentOutputInfo
  { resolution :: Resolution
  , tasks :: [Task]
  }

instance From AgentOutput AgentOutputInfo where
  from AgentOutput{..} = AgentOutputInfo{..}

instance Pretty AgentOutputInfo where
  pretty AgentOutputInfo{..} =
    pretty resolution <> case tasks of
      [] -> ""
      _  -> "\n\nRemaining tasks:\n" <> pretty tasks

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
