module Agent.Bob.Effect.Clock where

import Agent.Bob.Prelude
import Data.Time.Clock (UTCTime, getCurrentTime)

data Clock :: Effect where
  GetTime :: Clock f UTCTime
makeEffectF ''Clock

runClock :: (Emb IO :> es) => Eff (Clock : es) ~> Eff es
runClock = interpret \case
  GetTime -> liftIO getCurrentTime
