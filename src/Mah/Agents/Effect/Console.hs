module Mah.Agents.Effect.Console where

import Control.Monad (unless, when)
import Control.Monad.Hefty (Effect, Eff, Emb, makeEffectF, (:>), type (~>), interpret, liftIO)
import Data.Effect.Reader (Ask, asks)
import Data.Effect.State (State, gets, modify)
import Data.Text (Text)
import Data.Text qualified as Text
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.HarnessState (HarnessState(..))
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Data.UserInput (UserInput(..))
import Mah.Agents.Pretty qualified as Pretty
import Witch (From(..))
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
        Assistant -> "Agent"
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
