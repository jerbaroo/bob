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
      let personaStr = "You are an intelligent assistant responsible for tracking tasks and conversing with the user. Be helpful, clear, and concise. The task list is your working memory: use it to keep structure about what you are doing and plan to do."
          tasksStr [] = "You currently have no pending tasks."
          tasksStr tasks' =
               "The following tasks are currently pending. Consider the user's input and decide which remain:\n"
            <> Text.intercalate "\n" (("- " <>) . from <$> tasks')
          responseFormatStr = Text.unlines
            [ "### CRITICAL INSTRUCTION: OUTPUT FORMAT"
            , "You must respond with a single, raw JSON object and nothing else."
            , "The object has exactly two keys:"
            , "- \"remaining_tasks\": an array of strings representing your up-to-date task list."
            , "  Maintain it actively: ADD new tasks (whether requested by the user or that you"
            , "  identify as needed to make progress), KEEP tasks that are still pending, and REMOVE"
            , "  tasks that are now complete or no longer relevant. Use an empty array [] when nothing remains."
            , "- \"message\": a string containing your conversational reply to the user."
            , ""
            , "Example of a valid response:"
            , "{\"remaining_tasks\": [\"Buy milk\", \"Call the dentist\"], \"message\": \"I've added those two tasks for you.\"}"
            , ""
            , "Strict rules for your response:"
            , "- Output ONLY the JSON object. Do not include any text before or after it."
            , "- Do NOT wrap the JSON object itself in Markdown code fences (no ``` or ```json around the object)."
            , "- Do NOT include comments or explanations outside the JSON object."
            , "- The first character of your response must be '{' and the last character must be '}'."
            , "- Both \"remaining_tasks\" and \"message\" keys are required."
            , "- Markdown IS allowed inside the \"message\" string value (e.g. you may use code fences, lists, or emphasis within your reply); just ensure it is properly JSON-escaped as a string."
            ]
          content = Text.intercalate "\n\n" $
            [ personaStr
            , tasksStr tasks
            , responseFormatStr
            ]
      in  Message {role=System, thinking=Nothing, ..}

    userMessage :: Message
    userMessage =
      Message { content=from userInput, role=User, thinking=Nothing }
