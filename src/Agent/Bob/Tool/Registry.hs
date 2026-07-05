module Agent.Bob.Tool.Registry where

import Agent.Bob.Data.ToolName (ToolName)
import Agent.Bob.Prelude
import Agent.Bob.Tool (IsTool(..), Tool(..), toolName)
import Agent.Bob.Tool.ReadFiles (ReadFiles)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson

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
