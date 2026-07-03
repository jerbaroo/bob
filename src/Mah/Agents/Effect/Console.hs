module Mah.Agents.Effect.Console where

import Control.Monad.Hefty (Effect, Eff, Emb, makeEffectF, (:>), type (~>), interpret, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
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

separator :: Text
separator = "------------------------------------------------------------------"

readUserInput :: (Console :> es) => Eff es UserInput
readUserInput = do
  writeSeparator User
  writeConsole "> "
  readConsole

writeConsoleLn :: (Console :> es) => Role -> Text -> Eff es ()
writeConsoleLn role message = do
  writeSeparator role
  writeConsole $ Text.stripEnd message <> "\n"

writeSeparator :: (Console :> es) => Role -> Eff es ()
writeSeparator role =
  let roleText = case role of
        Assistant -> "Agent"
        System -> "System"
        User -> "You"
      roleLen = Text.length roleText
  in  writeConsole $ Pretty.colour Pretty.cyanDim
        $  "\n- "
        <> roleText
        <> " "
        <> Text.drop (roleLen + 3) separator -- +3 for two spaces and one '-'.
        <> "\n"
