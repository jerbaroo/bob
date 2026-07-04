module Mah.Agents.Tool.Registry where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
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

-- | Iterate over 'knownTools', until a matched name is found, then parse it.
parseToolRequest :: ToolName -> Aeson.Value -> Aeson.Parser Tool
parseToolRequest name args = go knownTools
 where
  go [] = fail $ "Unknown tool: " <> from name
  go (SomeTool (_ :: Proxy t) : rest)
    | toolName @t == name = Tool <$> (Aeson.parseJSON args :: Aeson.Parser t)
    | otherwise           = go rest

-- | Prompt descriptions for all known tools.
toolDescriptions :: [Text]
toolDescriptions =
  [ toolDescription @p | SomeTool (_ :: Proxy p) <- knownTools ]
