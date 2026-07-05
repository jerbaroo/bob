module Agent.Bob.Data.ToolResult where

import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.ToolName (ToolName(..))
import Agent.Bob.Prelude
import Agent.Bob.Prompt (ToPrompt(..))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Test.QuickCheck.Instances ()

-- | The result of running one tool the model requested.
data ToolResult = ToolResult
  { toolName :: ToolName
  , output   :: Text
  } deriving (Eq, Show)

instance Arbitrary ToolResult where
  arbitrary = ToolResult <$> (ToolName <$> arbitrary) <*> arbitrary

instance Aeson.FromJSON ToolResult where
  parseJSON = Aeson.withObject "ToolResult" \o ->
    ToolResult <$> o .: "tool" <*> o .: "output"

instance Aeson.ToJSON ToolResult where
  toJSON ToolResult{..} = Aeson.object
    [ ("tool", Aeson.toJSON toolName)
    , ("output", Aeson.toJSON output)
    ]

instance IsEvent ToolResult where
  eventTasks _ = Nothing
  eventToMessage result =
    Just Message{content=toPrompt result, role=User, thinking=Nothing}

-- | A delimited block, so multi-line output can't be confused with
-- surrounding prompt text.
instance ToPrompt ToolResult where
  toPrompt ToolResult{..} =
    "<tool_result tool=\"" <> into @Text toolName <> "\">\n"
      <> output
      <> "\n</tool_result>"
