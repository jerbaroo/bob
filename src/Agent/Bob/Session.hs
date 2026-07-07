module Agent.Bob.Session where

import Agent.Bob.Data.AgentId (AgentId)
import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Event.Registry (LineResult(..), readEventLine)
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (Console, writeSection)
import Agent.Bob.Effect.File (File, readFileLogErr, resolve)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Data.Map qualified as Map
import Data.Ord (Down(..))
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)

-- | A fresh session file name: @<root>_<branch>_<timestamp>_<uuid>@, or just
-- @<timestamp>_<uuid>@ outside a git repository (branch "default"). The
-- timestamp prefix makes a name sort chronological.
mkSessionName :: Text -> Text -> UTCTime -> Text -> Text
mkSessionName rootName branch time uuid =
  Text.intercalate "_" $ scope <> [stamp, uuid]
 where
  stamp = from $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" time
  scope = case branch of
    "default" -> []
    _         -> [sanitise rootName, sanitise branch]

-- | Underscores separate the name's fields, so they may not appear within one.
sanitise :: Text -> Text
sanitise = Text.replace "_" "-" . Text.replace "/" "-"

-- | The sessions reachable from this branch, newest first: the branch's own
-- (auto-named @<root>_<branch>_...@) plus any custom-named (@/name@d) ones.
-- Other branches' auto-named sessions are excluded.
branchSessions :: Text -> Text -> [Text] -> [Text]
branchSessions rootName branch = sortOn Down . filter relevant
 where
  relevant name = scope `Text.isPrefixOf` name
    || not (autoNamed `Text.isPrefixOf` name)
  autoNamed = sanitise rootName <> "_"
  scope = case branch of
    "default" -> ""
    _         -> autoNamed <> sanitise branch <> "_"

-- | The path (under @memoryPath@) of a fresh session, retrying on the
-- unlikely collision. Plain IO: runs at startup, before the effect stack.
newSessionPath :: Config -> IO Text
newSessionPath config = do
  time <- getCurrentTime
  let rootName = from $ takeFileName $ from @Text config.projectRoot
      attempt = do
        uuid <- UUID.nextRandom
        let path = config.memoryPath <> "/"
              <> mkSessionName rootName config.gitBranch time (UUID.toText uuid)
        exists <- doesFileExist =<< resolve (from config.projectRoot) (from path)
        if exists then attempt else pure path
  attempt

-- | Load the session history at @path@ into the agent's context, replacing
-- whatever is there. Lines that don't parse as a known event are skipped;
-- malformed lines are reported.
restoreSession
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => AgentId -> Text -> Eff es ()
restoreSession agentId path = do
  contents <- readFileLogErr path
  let results = map readEventLine $ filter (not . Text.null . Text.strip) $
        maybe [] Text.lines contents
      restored = [e | Restored e <- results]
      malformed = length [() | Malformed <- results]
  modify $ \HarnessState{..} -> HarnessState
    { events = Map.insert agentId (reverse restored) events
    , tasks = maybe tasks (Map.singleton agentId) $
        lastMay (mapMaybe eventTasks restored)
    , ..
    }
  writeSection System Info $ case length restored of
    0 -> "No session history."
    1 -> "Loaded session history (1 entry)."
    n -> "Loaded session history (" <> pretty n <> " entries)."
  when (malformed > 0) $ writeSection System Error $
    "Skipped " <> pretty malformed <> " malformed session-history line(s)."
