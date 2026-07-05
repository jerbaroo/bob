module Agent.Bob.Effect.Console where

import Agent.Bob.Prelude
import Data.Text qualified as Text
import Agent.Bob.Data.Config (Config(..))
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.UserInput (UserInput(..))
import Agent.Bob.Pretty qualified as Pretty
import System.IO (hFlush, stdout)

data Console :: Effect where
  -- | Read from the console.
  ReadConsole :: Console f UserInput
  -- | Write to the console.
  WriteConsole :: Text -> Console f ()
makeEffectF '' Console

runConsole :: Emb IO :> es => Eff (Console : es) ~> Eff es
runConsole = interpret \case
  ReadConsole -> liftIO $ from <$> getLine
  WriteConsole text -> liftIO do
    putStr $ from text
    hFlush stdout

-- * Wrappers.

readUserInput :: (Console :> es, State HarnessState :> es) => Eff es UserInput
readUserInput = do
  writeSeparator User
  modify $ \s -> s { loggingRole = Just User }
  writeConsole "> "
  readConsole

-- | Note, this is not a thread-safe operation!
writeSection
  :: (Ask Config :> es, Console :> es, State HarnessState :> es)
  => Role -> Level -> Text -> Eff es ()
writeSection role messageLevel message = do
  logLevel <- asks (.logLevel)
  previousRole <- gets (.loggingRole)
  when (logLevel >= messageLevel) do
    let sameRole = Just role == previousRole
    -- Only print a separator if the role is different.
    unless sameRole $ writeSeparator role
    let vSep = if sameRole then "\n" else ""
    writeConsole $ vSep <> Text.stripEnd message <> "\n"
    modify $ \s -> s{loggingRole=Just role}

writeSeparator :: (Console :> es) => Role -> Eff es ()
writeSeparator role =
  let roleText = case role of
        Assistant -> "Agent Bob"
        System -> "System"
        User -> "You"
      roleLen = Text.length roleText
      separator = "------------------------------------------------------------"
  in  writeConsole $ Pretty.colour Pretty.cyanDim
        $  "\n- "
        <> roleText
        <> " "
        <> Text.drop (roleLen + 3) separator -- +3 for two spaces and one '-'.
        <> "\n"
