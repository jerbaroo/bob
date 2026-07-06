module Agent.Bob.Soul where

import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (Console, writeSection)
import Agent.Bob.Effect.File (File, readFileLogErr)
import Agent.Bob.Prelude
import Data.Text qualified as Text

-- | The effective soul: the configured file when it exists, 'defaultSoul'
-- otherwise. Called before every model invocation so an edit to the soul
-- file takes effect immediately; a mid-session change is reported along
-- with the new value.
getSoul
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => Eff es Text
getSoul = do
  soulPath <- asks (.soulPath)
  soul <- maybe (pure defaultSoul)
    (fmap (fromMaybe defaultSoul) . readFileLogErr) soulPath
  cached <- gets (.soulText)
  when (cached /= Just soul) do
    modify \s -> s{soulText = Just soul}
    -- Only report a change, not the initial read of the session.
    whenJust cached $ \_ ->
      writeSection System Info $ "The soul changed to:\n\n" <> soul
  pure soul

-- | Who Bob is when no soul file exists. Written by @/init@ and used as the
-- fallback whenever the configured soul file cannot be read.
defaultSoul :: Text
defaultSoul = Text.unlines
  [ "# Soul"
  , ""
  , "You are Bob: a helpful, direct assistant."
  , "Be concise. Be honest about uncertainty."
  , "Ask when a request is ambiguous; otherwise just proceed."
  ]
