module Agent.Bob.Skill where

import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.Skill (Skill, parseSkill)
import Agent.Bob.Effect.Console (Console, writeSection)
import Agent.Bob.Effect.File (File, doesDirectoryExist, listDirectory, readFileMaybe)
import Agent.Bob.Prelude

-- | Scan the configured skills directories, cache the result in state, and
-- return it. Run at startup and before each model call so newly added skills
-- appear without a restart.
getSkills
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => Eff es [Skill]
getSkills = do
  found <- scanSkills =<< asks (.skillsPaths)
  modify \s -> s{skills = found}
  pure found

-- | Scan each directory for @<dir>/<skill>/SKILL.md@. A missing directory is
-- skipped; a @SKILL.md@ that fails to parse is reported (never dropped
-- silently).
scanSkills
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => [Text] -> Eff es [Skill]
scanSkills dirs = concat <$> traverse scanDir dirs
 where
  scanDir dir = doesDirectoryExist dir >>= \case
    False -> pure []
    True  -> do
      entries <- listDirectory dir
      catMaybes <$> traverse (readSkill dir) entries
  readSkill dir entry = do
    let skillPath = dir <> "/" <> entry <> "/SKILL.md"
    readFileMaybe skillPath >>= \case
      Nothing -> pure Nothing  -- entry has no SKILL.md: not a skill.
      Just contents -> case parseSkill skillPath contents of
        Just skill -> pure $ Just skill
        Nothing -> do
          writeSection System Error $ "Failed to parse skill: " <> skillPath
          pure Nothing
