module Agent.Bob.Effect.File where

import Control.Exception (IOException, try)
import Control.Monad.Hefty (Effect, Eff, Emb, makeEffectF, (:>), type (~>), interpret, liftIO)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO qualified as IO
import Witch (from)

data File :: Effect where
  AppendFile  :: Text -> Text -> File f ()
  ReadFile    :: Text -> File f (Maybe Text)
  ResolvePath :: Text -> File f Text
makeEffectF ''File

runFile :: (Emb IO :> es) => Eff (File : es) ~> Eff es
runFile = interpret \case
  ReadFile filePath -> liftIO do
    expandedPath <- expandTilde $ from filePath
    result <- try @IOException $ IO.readFile (from expandedPath)
    pure $ either (const Nothing) (Just . from) result
  AppendFile filePath content -> liftIO do
    expandedPath <- expandTilde $ from filePath
    createDirectoryIfMissing True $ takeDirectory expandedPath
    IO.appendFile expandedPath $ from content
  ResolvePath filePath -> liftIO $
    from <$> (expandTilde (from filePath) >>= makeAbsolute)

-- | Expands a leading tilde (~) in a filepath to the user's home directory.
expandTilde :: FilePath -> IO FilePath
expandTilde "~" = getHomeDirectory
expandTilde ('~' : '/' : rest) = (</> rest) <$> getHomeDirectory
expandTilde path = pure path
