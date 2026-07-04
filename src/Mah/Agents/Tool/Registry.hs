module Mah.Agents.Tool.Registry where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.List.Extra (firstJust)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Mah.Agents.Data.ToolName (ToolName)
import Mah.Agents.Tool (IsTool(..), Tool(..))
import Mah.Agents.Tool.ReadFiles (ReadFiles)
import Witch (from)

-- | An existential over a tool type (not a value).
data SomeTool
  = forall t. (Aeson.FromJSON t, Aeson.ToJSON t, IsTool t, Show t)
  => SomeTool (Proxy t)

-- | Every tool the harness knows how to run.
knownTools :: [SomeTool]
knownTools = [SomeTool $ Proxy @ReadFiles]

-- | The first of 'knownTools' whose name matches, with its arguments parsed,
-- wrapped as a 'Tool' ready to run.
parseToolRequest :: ToolName -> Aeson.Value -> Aeson.Parser Tool
parseToolRequest name args =
  fromMaybe (fail $ "Unknown tool: " <> from name) $ firstJust parse knownTools
 where
  parse (SomeTool (_ :: Proxy t))
    | toolName @t == name = Just $ Tool <$> Aeson.parseJSON @t args
    | otherwise           = Nothing

-- | Prompt descriptions for all known tools.
toolDescriptions :: [Text]
toolDescriptions =
  [ toolDescription @p | SomeTool (_ :: Proxy p) <- knownTools ]
