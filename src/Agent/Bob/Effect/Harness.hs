module Agent.Bob.Effect.Harness where

import Agent.Bob.Data.Config (Config)
import Agent.Bob.Data.HarnessState (HarnessState)
import Agent.Bob.Effect.Agent (Agent)
import Agent.Bob.Effect.Clock (Clock)
import Agent.Bob.Effect.Console (Console)
import Agent.Bob.Effect.File (File)
import Agent.Bob.Effect.Permission (Permission)
import Agent.Bob.Prelude

-- | The effects every command is allowed to use.
type Harness es =
  ( Agent :> es
  , Ask Config :> es
  , Clock :> es
  , Console :> es
  , File :> es
  , Permission :> es
  , State HarnessState :> es
  )
