module Agent.Bob.Effect.Agent where

import Control.Monad.Hefty (Effect, Eff, Emb, makeEffectF, (:>), type (~>), interpret, liftIO)
import Data.Effect.Reader (Ask, ask)
import Agent.Bob.Data.AgentInput (AgentInput)
import Agent.Bob.Data.AgentOutput (AgentOutput(..))
import Agent.Bob.Data.Config (Config(..))
import Agent.Bob.Prompt.Build (buildPrompt)
import Network.HTTP.Request (Response(body))
import Network.HTTP.Client (newManager, managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Request (Request(..), Method(POST), sendWith)
import Witch (from)

data Agent :: Effect where
  CallAgent :: AgentInput -> Agent f AgentOutput
makeEffectF ''Agent

runAgent :: (Ask Config :> es, Emb IO :> es) => Eff (Agent : es) ~> Eff es
runAgent = interpret \case
  CallAgent agentInput -> do
    config <- ask
    response <- liftIO $ do
      -- TODO: connection re-use.
      manager <- newManager $ tlsManagerSettings
        { managerResponseTimeout =
            responseTimeoutMicro $ config.timeoutSeconds * 1_000_000
        }
      let request = Request
            { method = POST
            , url = from config.engineUrl
            , headers = []
            , body = Just $ buildPrompt agentInput
            }
      sendWith manager request
    pure response.body
