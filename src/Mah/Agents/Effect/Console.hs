module Mah.Agents.Effect.Console where

import Control.Monad.Hefty (Effect, Eff, Emb, makeEffectF, (:>), type (~>), interpret, liftIO)
import Data.Text (Text)
import Mah.Agents.Data.UserInput (UserInput(..))
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
  writeConsole "> "
  readConsole

writeConsoleLn :: (Console :> es) => Text -> Eff es ()
writeConsoleLn = writeConsole . (<> "\n")
