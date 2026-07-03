module Mah.Agents.Data.AgentOutput where

import Control.Monad (unless)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Mah.Agents.Data.Event (IsEvent (..))
import Mah.Agents.Data.FinishReason (FinishReason (..))
import Mah.Agents.Data.Message (Message(..))
import Mah.Agents.Data.Resolution (Resolution)
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Data.Task (Task)
import Mah.Agents.Data.Thinking (Thinking(..))
import Mah.Agents.Pretty (Pretty(..))
import Witch (From(..))
import System.IO.Unsafe (unsafePerformIO)

-- | The final output from the agent.
data AgentOutput = AgentOutput
  { agentOutputMeta :: AgentOutputMeta
  , raw :: Aeson.Object
  , resolution :: Resolution
  , tasks :: [Task]
  , thinking :: Maybe Thinking
  } deriving Show

instance Aeson.FromJSON AgentOutput where
  parseJSON = Aeson.withObject "AgentOutput" $ \o' -> do
    let o = unsafePerformIO do
          putStrLn $ from $ pretty o'
          pure o'
    choice <- o .: "choices" >>= \case
      [choice] -> pure choice
      x -> fail $ "Expected exactly one choice, got: " <> show x
    (message :: Message) <- choice .: "message"
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

instance IsEvent AgentOutput where
  eventTasks AgentOutput{..} = Just tasks

instance Pretty AgentOutput where
  pretty AgentOutput{} = "AgentOutput{..}" -- Too verbose to print anymore.

-- | The schema we told the LLM to use inside the "content" string.
--
-- This exists just as a helper for parsing 'AgentOutput'.
data OutputContent = OutputContent
  { resolution :: Resolution
  , tasks      :: [Task]
  } deriving Show

instance Aeson.FromJSON OutputContent where
  parseJSON = Aeson.withObject "OutputContent" \o -> do
    resolution <- o .: "message"
    tasks      <- o .: "remaining_tasks"
    pure OutputContent{..}

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
