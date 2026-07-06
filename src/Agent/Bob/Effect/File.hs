module Agent.Bob.Effect.File where

import Agent.Bob.Data.Config (Config)
import Agent.Bob.Data.HarnessState (HarnessState)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (Console, writeSection)
import Agent.Bob.Prelude hiding (readFile)
import Control.Exception (IOException, try)
import Data.Text.IO qualified as Text.IO
import System.Directory (createDirectoryIfMissing, getHomeDirectory, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)

data File :: Effect where
  AppendFile     :: Text -> Text -> File f ()
  -- | Read a file, surfacing the 'IOException' on failure so callers can tell a
  -- missing file apart from a real error.
  ReadFile :: Text -> File f (Either IOException Text)
  ResolvePath    :: Text -> File f Text
  WriteFile      :: Text -> Text -> File f ()
makeEffectF ''File

-- | Interpret file operations with paths resolved against the given project
-- root (see 'resolve').
--
-- Reads use strict 'Data.Text.IO' so the handle is fully read and closed before
-- we return. Lazy 'System.IO.readFile' would keep the handle semi-closed until
-- the contents were forced by the caller; if nothing forced them (e.g. a
-- filtered-out log message) a later 'AppendFile' to the same path could crash
-- with "resource busy (file is locked)". Strictness also keeps read errors
-- inside the 'try' rather than letting them escape at force time.
runFile :: (Emb IO :> es) => FilePath -> Eff (File : es) ~> Eff es
runFile root = interpret \case
  ReadFile filePath -> liftIO do
    path <- resolve root $ from filePath
    try @IOException $ Text.IO.readFile path
  AppendFile filePath content -> liftIO do
    path <- resolve root $ from filePath
    createDirectoryIfMissing True $ takeDirectory path
    Text.IO.appendFile path content
  ResolvePath filePath -> liftIO $ from <$> resolve root (from filePath)
  WriteFile filePath content -> liftIO do
    path <- resolve root $ from filePath
    createDirectoryIfMissing True $ takeDirectory path
    Text.IO.writeFile path content

-- | Read a file, collapsing any failure (missing or unreadable) to 'Nothing'
-- without reporting it.
readFileMaybe :: File :> es => Text -> Eff es (Maybe Text)
readFileMaybe = fmap (either (const Nothing) Just) . readFile

-- | Like 'readFileMaybe', but logs read errors.
readFileLogErr
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => Text -> Eff es (Maybe Text)
readFileLogErr filePath = readFile filePath >>= \case
  Right contents -> pure $ Just contents
  Left e
    | isDoesNotExistError e -> pure Nothing
    | otherwise -> do
        writeSection System Error $
          "Failed to read " <> filePath <> ": " <> from (show e)
        pure Nothing

-- | Expand a leading tilde, then interpret relative paths against the project
-- root rather than the CWD ('</>' keeps its second argument when absolute).
resolve :: FilePath -> FilePath -> IO FilePath
resolve root path = expandTilde path >>= makeAbsolute . (root </>)

-- | Expands a leading tilde (~) in a filepath to the user's home directory.
expandTilde :: FilePath -> IO FilePath
expandTilde "~" = getHomeDirectory
expandTilde ('~' : '/' : rest) = (</> rest) <$> getHomeDirectory
expandTilde path = pure path
