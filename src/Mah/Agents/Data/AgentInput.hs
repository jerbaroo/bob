module Mah.Agents.Data.AgentInput where

import Data.Aeson as Aeson
import Data.Maybe (catMaybes)
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Mah.Agents.Data.Task (Task)
import Mah.Agents.Data.Event (Event)
import Mah.Agents.Data.Message (Message(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Data.Temperature (Temperature(..))
import Mah.Agents.Data.UserInput (UserInput(..))
import Witch (from, tryInto)

-- | Input available to an agent.
data AgentInput = AgentInput
  { events :: [Event]
  -- ^ History of events.
  --   May be a compressed version of the full history.
  , model :: Text
  , tasks :: [Task]
  -- ^ Current tasks to complete.
  , temperature :: Temperature
  , userInput :: UserInput
  }

instance Aeson.ToJSON AgentInput where
  toJSON AgentInput{..} =
    Aeson.object
      [ ("model", Aeson.toJSON model)
      , ("messages", Aeson.toJSON messages)
      , ("response_format", Aeson.object [("type", "json_object")])
      , ("temperature", Aeson.toJSON temperature)
      ]
   where
    eventMessages :: [Message]
    eventMessages = catMaybes $ eitherToMaybe . tryInto @Message <$> events

    messages :: [Message]
    messages = systemMessage : eventMessages <> [userMessage]

    systemMessage :: Message
    systemMessage =
      let tasksStr [] = Nothing
          tasksStr tasks' = Just $
               "Your current tasks are:"
            <> Text.intercalate "\n" (("- " <>) . from <$> tasks')
          responseFormatStr =
               "CRITICAL INSTRUCTION: You must respond with a raw JSON object matching exactly this schema:\n"
            <> "{\n"
            <> "  \"remaining_tasks\": [\"A list of strings representing the tasks you still need to do\"],\n"
            <> "  \"message\": \"Your actual conversational reply to the user\"\n"
            <> "}"
          content = Text.intercalate "\n\n" $ catMaybes
            [ tasksStr tasks
            , Just responseFormatStr
            ]
      in  Message {role=System, thinking=Nothing, ..}

    userMessage :: Message
    userMessage =
      Message { content=from userInput, role=User, thinking=Nothing }
