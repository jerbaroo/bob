module Agent.Bob.Data.FinishReason where

import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Agent.Bob.Pretty (Pretty(..))

data FinishReason
  = Stop
  | Length
  | ToolCalls
  | ContentFilter
  | Other Text  -- ^ Catch-all so an unknown reason never fails parsing.
  deriving Show

finishReasonText :: FinishReason -> Text
finishReasonText = \case
  Stop          -> "stop"
  Length        -> "length"
  ToolCalls     -> "tool_calls"
  ContentFilter -> "content_filter"
  Other t       -> t

instance Aeson.FromJSON FinishReason where
  parseJSON = Aeson.withText "FinishReason" $ pure . \case
    "stop"           -> Stop
    "length"         -> Length
    "tool_calls"     -> ToolCalls
    "content_filter" -> ContentFilter
    x                -> Other x

instance Aeson.ToJSON FinishReason where
  toJSON = Aeson.toJSON . finishReasonText

instance Pretty FinishReason where
  pretty = finishReasonText
