module Mah.Agents.Data.ToolResult where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Mah.Agents.Data.Event (IsEvent(..))
import Mah.Agents.Prompt (ToPrompt(..))
import Mah.Agents.Tool (Tool, toolName')
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

instance ToPrompt ToolResult where
  toPrompt ToolResult{..} = into @Text (toolName' tool) <> ":\n" <> output
