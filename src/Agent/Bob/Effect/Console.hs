module Agent.Bob.Effect.Console where

import Agent.Bob.Prelude
import Data.Text qualified as Text
import Agent.Bob.Data.Config (Config, ConfigF(..))
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

readUserInput
  :: (Ask Config :> es, Console :> es, State HarnessState :> es)
  => Eff es UserInput
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
    writeConsole $ vSep <> prefix messageLevel <> Text.stripEnd message <> "\n"
    modify $ \s -> s{loggingRole=Just role}

-- | Tag error/debug lines so their severity is visible in the transcript.
-- Only the first line is prefixed. Other levels pass through untouched.
prefix :: Level -> Text
prefix Error = Pretty.colour Pretty.red "[error] "
prefix Debug = Pretty.colour Pretty.grey "[debug] "
prefix _     = ""

writeSeparator :: (Ask Config :> es, Console :> es) => Role -> Eff es ()
writeSeparator role = do
  modelName <- asks (.modelName)
  let roleText = case role of
        Assistant -> "Bob (" <> modelName <> ")"
        System -> "Bob"
        User -> "You"
      roleLen = Text.length roleText
      separator = "------------------------------------------------------------"
  writeConsole $ Pretty.colour Pretty.cyanDim
    $  "\n- "
    <> roleText
    <> " "
    <> Text.drop (roleLen + 3) separator -- +3 for two spaces and one '-'.
    <> "\n"
