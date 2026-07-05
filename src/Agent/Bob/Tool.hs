module Agent.Bob.Tool where

import Agent.Bob.Data.ToolName (ToolName)
import Agent.Bob.Effect.File (File)
import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson

data Tool
  = forall t
  . (Aeson.FromJSON t, Aeson.ToJSON t, IsTool t, Show t)
  => Tool t

instance Aeson.ToJSON Tool where
  toJSON (Tool t) = Aeson.toJSON t

instance Show Tool where
  show (Tool t) = show t

-- | Something the agent can ask the harness to do on its behalf.
class Typeable t => IsTool t where
  type ToolOutput t
  -- | Decide whether a single permission grant covers this invocation.
  permissionGranted :: (File :> es) => t -> Text -> Eff es Bool
  -- | Render the tool output for the model.
  renderOutput :: t -> ToolOutput t -> Text
  -- | Run a tool to produce a result.
  runTool :: (File :> es) => t -> Eff es (ToolOutput t)
  -- | Human/model readable description injected into the prompt,
  toolDescription :: Text

-- | The name the model uses to request a tool, derived from the type name.
toolName :: forall t. Typeable t => ToolName
toolName = from $ showTyCon @t

toolName' :: Tool -> ToolName
toolName' (Tool (_ :: t)) = toolName @t

-- | Run a tool and render its output to 'Text'.
runToolForModel :: (File :> es) => Tool -> Eff es Text
runToolForModel (Tool t) = renderOutput t <$> runTool t
