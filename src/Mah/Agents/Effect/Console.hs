module Mah.Agents.Effect.Console where

import Control.Monad (when)
import Control.Monad.Hefty (Effect, Eff, Emb, makeEffectF, (:>), type (~>), interpret, liftIO)
import Data.Effect.Reader (Ask, asks)
import Data.Text (Text)
import Data.Text qualified as Text
import Mah.Agents.Data.Config (Config(..))
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

readUserInput :: (Console :> es) => Eff es UserInput
readUserInput = do
  writeSeparator User
  writeConsole "> "
  readConsole

writeSection :: (Ask Config :> es, Console :> es) => Level -> Role -> Text -> Eff es ()
writeSection level role message = do
  level' <- asks (.level)
  when (level' >= level) do
    writeSeparator role
    writeConsole $ Text.stripEnd message <> "\n"

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
