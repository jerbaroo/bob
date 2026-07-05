module Agent.Bob.ProjectRoot where

import Agent.Bob.Prelude
import System.Directory (doesPathExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))

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
