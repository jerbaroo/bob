module Agent.Bob.ProjectRoot where

import Agent.Bob.Prelude
import Control.Exception (IOException, try)
import Data.Text qualified as Text
import System.Directory (doesDirectoryExist, doesPathExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO qualified as IO

-- | The directory under the project root holding Bob's per-project files
-- (soul, memory, skills, ...): @<root>/agent-bob/@.
projectDir :: FilePath
projectDir = "agent-bob"

-- | Find the git project root by walking up from the given directory looking
-- for a @.git@ entry. 'Nothing' when no ancestor contains one (not a repo).
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot dir = do
  absDir <- makeAbsolute dir
  -- @.git@ is a directory in a normal checkout but a file in worktrees and
  -- submodules, so check for any kind of entry.
  findProjectRootBy (doesPathExist . (</> ".git")) absDir

-- | The walk itself, with the "is this the root?" check injected so the logic
-- is testable without a filesystem.
findProjectRootBy
  :: Monad m => (FilePath -> m Bool) -> FilePath -> m (Maybe FilePath)
findProjectRootBy isRoot = go
 where
  go dir = isRoot dir >>= \case
    True  -> pure $ Just dir
    False ->
      let parent = takeDirectory dir
      in  if parent == dir then pure Nothing else go parent

-- | The key episodic memory is stored under, fixed at startup: the current
-- git branch (sanitised for use as a filename), the short commit SHA when
-- HEAD is detached, or @default@ outside a git repository.
sessionKey :: FilePath -> IO Text
sessionKey root = keyFromHead <$> readGitHead root

-- | Sanitise a parsed HEAD into a single filesystem-safe path component.
keyFromHead :: Maybe Text -> Text
keyFromHead = maybe "default" $ Text.replace "/" "-"

-- | The current branch name (or detached-HEAD short SHA) read from
-- @.git/HEAD@, without shelling out to git.
readGitHead :: FilePath -> IO (Maybe Text)
readGitHead root = do
  let dotGit = root </> ".git"
  gitDir <- doesDirectoryExist dotGit >>= \case
    True  -> pure $ Just dotGit
    -- In worktrees and submodules @.git@ is a file: @gitdir: <path>@, where
    -- a relative path is relative to the project root.
    False -> readFileMay dotGit <&> \contents -> do
      line <- Text.strip <$> contents
      rest <- Text.stripPrefix "gitdir:" line
      pure $ root </> from (Text.strip rest)
  case gitDir of
    Nothing  -> pure Nothing
    Just dir -> (parseHead =<<) <$> readFileMay (dir </> "HEAD")

-- | Parse the contents of @.git/HEAD@: the branch name for a symbolic ref,
-- the short SHA for a detached HEAD.
parseHead :: Text -> Maybe Text
parseHead contents
  | Just branch <- Text.stripPrefix "ref: refs/heads/" line = Just branch
  | Text.length line >= shortShaLength = Just $ Text.take shortShaLength line
  | otherwise = Nothing
 where
  line = Text.strip contents
  shortShaLength = 7

readFileMay :: FilePath -> IO (Maybe Text)
readFileMay path =
  either (const Nothing) (Just . from)
    <$> try @IOException (IO.readFile path)
