module Mah.Agents.Effect.Agent where

import Control.Monad.Hefty (Effect, Eff, Emb, makeEffectF, (:>), type (~>), interpret, liftIO)
import Data.Effect.Reader (Ask, ask)
import Mah.Agents.Data.AgentInput (AgentInput)
import Mah.Agents.Data.AgentOutput (AgentOutput(..))
import Mah.Agents.Data.Config (Config(..))
import Network.HTTP.Request (post, Response(body))
import Witch (from)

data Agent :: Effect where
  CallAgent :: AgentInput -> Agent f AgentOutput
makeEffectF ''Agent

runAgent :: (Ask Config :> es, Emb IO :> es) => Eff (Agent : es) ~> Eff es
runAgent = interpret \case
  CallAgent agentInput -> do
    config <- ask
    response <- liftIO $ post (from config.url) agentInput
    pure response.body
