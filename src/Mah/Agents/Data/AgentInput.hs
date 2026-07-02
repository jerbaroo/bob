module Mah.Agents.Data.AgentInput where

import Data.Aeson as Aeson
import Data.Maybe (catMaybes)
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Mah.Agents.Data.AgentTask (AgentTask)
import Mah.Agents.Data.Event (Event)
import Mah.Agents.Data.Message (Message, toMessage)
import Mah.Agents.Data.Message qualified as Message
import Mah.Agents.Data.Temperature (Temperature(..))
import Mah.Agents.Data.UserInput (UserInput(..))
import Witch (from, tryInto)

-- | Input available to an agent.
data AgentInput = AgentInput
  { events :: [Event]
  -- ^ History of events.
  --   May be a compressed version of the full history.
  , model :: Text
  , tasks :: [AgentTask]
  -- ^ Current tasks to complete.
  , temperature :: Temperature
  , userInput :: UserInput
  }

instance Aeson.ToJSON AgentInput where
  toJSON AgentInput{..} =
    Aeson.object
      [ ("model", Aeson.toJSON model)
      , ("messages", Aeson.toJSON messages)
      , ("temperature", Aeson.toJSON temperature)
      ]
   where
    eventMessages :: [Message]
    eventMessages = catMaybes $ eitherToMaybe . tryInto @Message <$> events

    messages :: [Message]
    messages = systemMessage : eventMessages <> [userMessage]

    systemMessage :: Message
    systemMessage = Message.toMessage "system" $
      "You are an AI agent. Your current tasks are:\n"
      <> Text.intercalate "\n" (("- " <>) . from <$> tasks)

    userMessage :: Message
    userMessage = toMessage "user" $ from userInput
