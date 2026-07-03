module Mah.Agents.Effect.File where

import Control.Monad.Hefty (Effect, Eff, Emb, makeEffectF, (:>), type (~>), interpret, liftIO)
import Data.Text (Text)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO qualified as IO
import Witch (from)

data File :: Effect where
  ReadFile :: Text -> File f (Maybe Text)
makeEffectF ''File

runFile :: (Emb IO :> es) => Eff (File : es) ~> Eff es
runFile = interpret \case
  ReadFile filePath -> liftIO do
    expandedPath <- expandTilde $ from filePath
    Just . from <$> IO.readFile (from expandedPath)

-- | Expands a leading tilde (~) in a filepath to the user's home directory.
expandTilde :: FilePath -> IO FilePath
expandTilde "~" = getHomeDirectory
expandTilde ('~' : '/' : rest) = (</> rest) <$> getHomeDirectory
expandTilde path = pure path
