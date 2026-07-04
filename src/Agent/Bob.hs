module Agent.Bob where

import Control.Monad (void)
import Control.Monad.Extra (whenJust)
import Control.Monad.Hefty (Eff, runEff)
import Data.Effect.Reader (asks, runAsk)
import Data.Effect.State (evalStateIORef, modify)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (readFile)
import Agent.Bob.Command (Command(..), Harness, step)
import Agent.Bob.Command.ReadConsole (ReadConsole(..))
import Agent.Bob.Data.AgentId (AgentId(..))
import Agent.Bob.Data.Config (Config(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Event.Registry (parseEventLine)
import Agent.Bob.Data.HarnessState (HarnessState(..), mkHarnessState)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Agent (runAgent)
import Agent.Bob.Effect.Console (runConsole, writeSection)
import Agent.Bob.Effect.File (readFile, runFile)
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.UserCommand.Registry (parseCommand)
import Safe (lastMay)
import System.Directory (getCurrentDirectory)
import Witch (from)

-- | Restore the session from the persisted event log, so the model starts
-- with the context of previous runs instead of starting fresh. Lines that
-- don't parse as a known event are skipped.
restoreSession :: Harness es => AgentId -> Eff es ()
restoreSession agentId = do
  path <- asks (.memoryPath)
  contents <- readFile path
  -- TODO log an error if any events failed to parse.
  let restored = maybe [] (mapMaybe parseEventLine . Text.lines) contents
  modify $ \HarnessState{..} -> HarnessState
    { events = Map.insert agentId (reverse restored) events
    , tasks = maybe tasks (Map.singleton agentId) $
        lastMay (mapMaybe eventTasks restored)
    , ..
    }
  writeSection System Meta $
    "Restored " <> pretty (length restored) <> " events."

-- | The main agentic loop: read user input, parse it into a command, execute.
run :: Harness es => AgentId -> Eff es ()
run agentId = do
  userInput <- step agentId ReadConsole
  whenJust (parseCommand userInput) \(Command command) ->
    void $ step agentId command
  run agentId

-- | Sessions are stored centrally but keyed by the directory the tool is
-- started from, so each project keeps its own history.
sessionPath :: FilePath -> Text
sessionPath cwd = "~/.mah-agents/sessions/" <> sessionName cwd

sessionName :: String -> Text
sessionName cwd = case Text.uncons $ Text.replace "/" "-" $ from cwd of
  Just ('-', rest) -> rest
  _ -> error $ "Current working directory should begin with /"

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let config = Config
        { engineUrl = "http://127.0.0.1:8080/v1/chat/completions"
        , logLevel = Debug
        , memoryPath = sessionPath cwd
        , modelName = "Qwen"
        , modelTemperature = 0.5
        , permissionsPath = "~/.mah-agents/permissions"
        , soulPath = Just "~/.config/mah-agents/SOUL.md"
        , timeoutSeconds = 10 * 60 -- 10 minutes.
        }
  let initialState = mkHarnessState config
  runEff . runAsk config . runAgent . runConsole . runFile . evalStateIORef initialState $ do
    writeSection System Info $ "Type /help for assistance.\n\n" <> pretty config
    let agentId = AgentId 0
    restoreSession agentId
    run agentId
