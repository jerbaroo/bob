module Mah.Agents.Command where

import Control.Monad.Hefty (Eff, (:>))
import Data.Aeson qualified as Aeson
import Data.Effect.Reader (Ask)
import Data.Effect.State (State)
import Mah.Agents.Data.AgentId (AgentId)
import Mah.Agents.Data.Config (Config)
import Mah.Agents.Data.Event (Event, IsEvent(..))
import Mah.Agents.Data.HarnessState (HarnessState)
import Mah.Agents.Effect.Agent (Agent)
import Mah.Agents.Effect.Console (Console)
import Mah.Agents.Effect.File (File)
import Mah.Agents.Pretty (Pretty(..))
import Witch (From(..))

-- | The effects every command is allowed to use.
type Harness es =
  ( Agent :> es
  , Ask Config :> es
  , Console :> es
  , File :> es
  , State HarnessState :> es
  )

-- | An existential wrapper over any concrete command.
data Command = forall c. (Aeson.ToJSON c, IsCommand c, IsEvent c, Pretty c) => Command c

instance (Aeson.ToJSON c, IsCommand c, IsEvent c, Pretty c) => From c Command where
  from = Command

instance Aeson.ToJSON Command where
  toJSON (Command c) = Aeson.toJSON c

instance IsEvent Command where
  eventName (Command c) = "Command:" <> eventName c
  eventTasks (Command c) = eventTasks c
  eventToMessage (Command c) = eventToMessage c

instance Pretty Command where
  pretty (Command c) = "Command: " <> pretty c

-- | Something the harness can do.
class IsCommand c where
  type Result c
  runCommand :: Harness es => AgentId -> c -> Eff es (Result c, [Event])
