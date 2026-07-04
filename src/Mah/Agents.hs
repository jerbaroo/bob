module Mah.Agents where

import Control.Monad (void)
import Control.Monad.Extra (whenJust)
import Control.Monad.Hefty (Eff, runEff)
import Data.Effect.Reader (runAsk)
import Data.Effect.State (evalStateIORef)
import Mah.Agents.Command (Command(..), Harness, step)
import Mah.Agents.Command.ReadConsole (ReadConsole(..))
import Mah.Agents.Data.AgentId (AgentId(..))
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.HarnessState (mkHarnessState)
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Effect.Agent (runAgent)
import Mah.Agents.Effect.Console (runConsole, writeSection)
import Mah.Agents.Effect.File (runFile)
import Mah.Agents.Pretty (Pretty(..))
import Mah.Agents.UserCommand.Registry (parseCommand)

-- | The main agentic loop: read user input, parse it into a command, execute.
run :: Harness es => AgentId -> Eff es ()
run agentId = do
  userInput <- step agentId ReadConsole
  whenJust (parseCommand userInput) \(Command command) ->
    void $ step agentId command
  run agentId

main :: IO ()
main = do
  let config = Config
        { engineUrl = "http://127.0.0.1:8080/v1/chat/completions"
        , logLevel = Debug
        , memoryPath = "~/.mah-agents/sessions/default"
        , modelName = "Qwen"
        , modelTemperature = 0.5
        , permissionsPath = "~/.mah-agents/permissions"
        , soulPath = Just "~/.config/mah-agents/SOUL.md"
        , timeoutSeconds = 10 * 60 -- 10 minutes.
        }
  let initialState = mkHarnessState config
  runEff . runAsk config . runAgent . runConsole . runFile . evalStateIORef initialState $ do
    writeSection System Info $ "Type /help for assistance.\n\n" <> pretty config
    run $ AgentId 0
