module Agent.Bob.Effect.File where

import Agent.Bob.Prelude
import Control.Exception (IOException, try)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO qualified as IO

data File :: Effect where
  AppendFile  :: Text -> Text -> File f ()
  ReadFile    :: Text -> File f (Maybe Text)
  ResolvePath :: Text -> File f Text
  WriteFile   :: Text -> Text -> File f ()
makeEffectF ''File

-- | Interpret file operations with paths resolved against the given project
-- root (see 'resolve').
runFile :: (Emb IO :> es) => FilePath -> Eff (File : es) ~> Eff es
runFile root = interpret \case
  ReadFile filePath -> liftIO do
    path <- resolve root $ from filePath
    result <- try @IOException $ IO.readFile path
    pure $ either (const Nothing) (Just . from) result
  AppendFile filePath content -> liftIO do
    path <- resolve root $ from filePath
    createDirectoryIfMissing True $ takeDirectory path
    IO.appendFile path $ from content
  ResolvePath filePath -> liftIO $ from <$> resolve root (from filePath)
  WriteFile filePath content -> liftIO do
    path <- resolve root $ from filePath
    createDirectoryIfMissing True $ takeDirectory path
    IO.writeFile path $ from content

-- | Expand a leading tilde, then interpret relative paths against the project
-- root rather than the CWD ('</>' keeps its second argument when absolute).
resolve :: FilePath -> FilePath -> IO FilePath
resolve root path = expandTilde path >>= makeAbsolute . (root </>)

-- | Expands a leading tilde (~) in a filepath to the user's home directory.
expandTilde :: FilePath -> IO FilePath
expandTilde "~" = getHomeDirectory
expandTilde ('~' : '/' : rest) = (</> rest) <$> getHomeDirectory
expandTilde path = pure path
