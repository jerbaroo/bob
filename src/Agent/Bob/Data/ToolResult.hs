module Agent.Bob.Data.ToolResult where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Prompt (ToPrompt(..))
import Agent.Bob.Tool (Tool, toolName')
import Witch (into)

-- | The result of running one tool the model requested.
data ToolResult = ToolResult
  { tool   :: Tool
  , output :: Text
  }

instance Aeson.ToJSON ToolResult where
  toJSON ToolResult{..} = Aeson.object
    [ ("tool", Aeson.toJSON tool)
    , ("output", Aeson.toJSON output)
    ]

instance IsEvent ToolResult where
  eventName _ = "ToolResult"

-- | A delimited block, so multi-line output can't be confused with
-- surrounding prompt text.
instance ToPrompt ToolResult where
  toPrompt ToolResult{..} =
    "<tool_result tool=\"" <> into @Text (toolName' tool) <> "\">\n"
      <> output
      <> "\n</tool_result>"
