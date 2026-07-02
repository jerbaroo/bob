module Mah.Agents.Effect.Agent where

import Control.Monad.Hefty (Effect, Eff, Emb, makeEffectF, (:>), type (~>), interpret)
import Data.Effect.Reader (Ask)
import Mah.Agents.Data.AgentInput (AgentInput)
import Mah.Agents.Data.AgentOutput (AgentOutput(..))
import Mah.Agents.Data.Config (Config)

data Agent :: Effect where
  CallAgent :: AgentInput -> Agent f AgentOutput
makeEffectF ''Agent

runAgent :: (Ask Config :> es, Emb IO :> es) => Eff (Agent : es) ~> Eff es
runAgent = interpret \case
  CallAgent _agentInput -> pure AgentOutput
    { tasks = ["A task", "Another task"]
    , thoughts = ["Thoughts hurt me."]
    , resolution = "I am thinking it would be nice to be real."
    }
