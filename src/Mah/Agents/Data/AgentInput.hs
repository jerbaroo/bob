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
import Mah.Agents.Data.ToolResult (ToolResult)
import Mah.Agents.Data.UserInput (UserInput(..))
import Mah.Agents.Pretty (Pretty(..))
import Mah.Agents.Tool.Registry (toolDescriptions)
import Witch (from, tryInto)

-- | Input available to an agent.
data AgentInput = AgentInput
  { events :: [Event]
  -- ^ History of events.
  --   May be a compressed version of the full history.
  , modelName :: Text
  , modelTemperature :: Temperature
  , modelSoul :: Maybe Text
  , modelTasks :: [Task]
  -- ^ Current tasks to complete.
  , userInput :: UserInput
  , toolResults :: [ToolResult]
  -- ^ Results of tools the model requested on the previous call, if any.
  --   The 'userInput' is kept unchanged while these accumulate.
  }

instance Aeson.ToJSON AgentInput where
  toJSON AgentInput{..} =
    Aeson.object
      [ ("messages", Aeson.toJSON messages)
      , ("model", Aeson.toJSON modelName)
      , ("response_format", Aeson.object [("type", "json_object")])
      , ("temperature", Aeson.toJSON modelTemperature)
      ]
   where
    eventMessages :: [Message]
    eventMessages = catMaybes $ eitherToMaybe . tryInto @Message <$> events

    messages :: [Message]
    messages = systemMessage : eventMessages <> [userMessage] <> toolResultMessages

    -- Tool results are appended as a fresh message so the original user input
    -- (above) stays intact for the whole tool-drain loop.
    toolResultMessages :: [Message]
    toolResultMessages = case toolResults of
      [] -> []
      results ->
        [ Message
            { role = User
            , thinking = Nothing
            , content =
                "The tools you requested have completed. Each result below is\
                \ labelled with the tool that produced it; use them to continue."
                <> pretty results
            }
        ]

    systemMessage :: Message
    systemMessage =
      let personaStr = "You are an intelligent assistant responsible for tracking tasks and conversing with the user. The task list is your working memory: use it to keep structure about what you are doing and plan to do."
          tasksStr [] = "You currently have no pending tasks."
          tasksStr tasks' =
               "The following tasks are currently pending. Consider the user's input and decide which remain:\n"
            <> pretty tasks'
          responseFormatStr = Text.unlines
            [ "### CRITICAL INSTRUCTION: OUTPUT FORMAT"
            , "You must respond with a single, raw JSON object and nothing else."
            , "The object has exactly two keys:"
            , "- \"remaining_tasks\": an array of strings representing your up-to-date task list."
            , "  Maintain it actively: ADD new tasks (whether requested by the user or that you"
            , "  identify as needed to make progress), KEEP tasks that are still pending, and REMOVE"
            , "  tasks that are now complete or no longer relevant. Use an empty array [] when nothing remains."
            , "- \"message\": a string containing your conversational reply to the user."
            , "- \"tool_calls\" (optional): an array of tool requests. Include it ONLY when you"
            , "  want to run a tool; each element is {\"name\": \"<ToolName>\", \"arguments\": {...}}."
            , "  When you request tools you will be called again with their results as the next input."
            , "  See AVAILABLE TOOLS below for what you may request."
            , ""
            , "Example of a valid response:"
            , "{\"remaining_tasks\": [\"Buy milk\", \"Call the dentist\"], \"message\": \"I've added those two tasks for you.\"}"
            , ""
            , "Strict rules for your response:"
            , "- Output ONLY the JSON object. Do not include any text before or after it."
            , "- Do NOT wrap the JSON object itself in Markdown code fences (no ``` or ```json around the object)."
            , "- Do NOT include comments or explanations outside the JSON object."
            , "- The first character of your response must be '{' and the last character must be '}'."
            , "- Both \"remaining_tasks\" and \"message\" keys are required; \"tool_calls\" is optional."
            , "- Markdown IS allowed inside the \"message\" string value (e.g. you may use code fences, lists, or emphasis within your reply); just ensure it is properly JSON-escaped as a string."
            ]
          availableToolsStr = Text.intercalate "\n"
            $ "### AVAILABLE TOOLS" : toolDescriptions
          content = Text.intercalate "\n\n" $ catMaybes
            [ Just personaStr
            , modelSoul
            , Just $ tasksStr modelTasks
            , Just responseFormatStr
            , Just availableToolsStr
            ]
      in  Message {role=System, thinking=Nothing, ..}

    userMessage :: Message
    userMessage =
      Message { content=from userInput, role=User, thinking=Nothing }

instance Pretty AgentInput
