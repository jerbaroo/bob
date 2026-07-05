module Agent.Bob.Prompt.Build where

import Data.Aeson qualified as Aeson
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Agent.Bob.Data.AgentInput (AgentInput(..))
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Prompt (ToPrompt(..))
import Agent.Bob.Tool.Registry (toolDescriptions)
import Witch (tryInto)

buildPrompt :: AgentInput -> Aeson.Value
buildPrompt AgentInput{..} =
  Aeson.object
    [ ("messages", Aeson.toJSON messages)
    , ("model", Aeson.toJSON modelName)
    , ("response_format", Aeson.object [("type", "json_object")])
    , ("temperature", Aeson.toJSON modelTemperature)
    ]
 where
  -- Events are stored newest-first; reverse into chronological order. The
  -- current user input is among them ('UserInput' is recorded before the
  -- agent is called), so no separate user message is added.
  eventMessages :: [Message]
  eventMessages = reverse . catMaybes $ eitherToMaybe . tryInto @Message <$> events

  messages :: [Message]
  messages = systemMessage : eventMessages <> toolResultMessages

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
              \ wrapped in a <tool_result> tag naming the tool that produced\
              \ it. Use the results to continue, responding in the usual JSON\
              \ format.\n\n"
              <> Text.intercalate "\n\n" (toPrompt <$> results)
          }
      ]

  systemMessage :: Message
  systemMessage =
    let personaStr = "You are an intelligent assistant responsible for tracking tasks and conversing with the user. The task list is your working memory: use it to keep structure about what you are doing and plan to do."
        tasksStr tasks' = Text.intercalate "\n" $ "### PENDING TASKS" : case tasks' of
          [] -> ["None."]
          _  ->
            [ "Consider the user's input and decide which of these remain:"
            , toPrompt tasks'
            ]
        responseFormatStr = Text.intercalate "\n"
          [ "### RESPONSE FORMAT (CRITICAL)"
          , "You must respond with a single, raw JSON object and nothing else."
          , "It has two required keys and one optional key:"
          , "- \"message\" (required): a string containing your conversational reply to the"
          , "  user. Markdown IS allowed inside this string (code fences, lists, emphasis);"
          , "  just ensure it is properly JSON-escaped."
          , "- \"remaining_tasks\" (required): an array of strings: your up-to-date version of"
          , "  PENDING TASKS above. Maintain it actively: ADD new tasks (whether requested by"
          , "  the user or that you identify as needed to make progress), KEEP tasks that are"
          , "  still pending, and REMOVE tasks that are now complete or no longer relevant."
          , "  Use an empty array [] when nothing remains."
          , "- \"tool_calls\" (optional): an array of tool requests, each of the form"
          , "  {\"name\": \"<ToolName>\", \"arguments\": {...}}. Include it ONLY when you need a"
          , "  tool to make progress; you will be called again with the results as the next"
          , "  input. Only the tools listed under AVAILABLE TOOLS below may be requested."
          , ""
          , "Example of a valid response:"
          , "{\"remaining_tasks\": [\"Buy milk\", \"Call the dentist\"], \"message\": \"I've added those two tasks for you.\"}"
          , ""
          , "Strict rules for your response:"
          , "- Output ONLY the JSON object: the first character of your response must be '{'"
          , "  and the last character must be '}'."
          , "- Do NOT wrap the object in Markdown code fences and do NOT include any text,"
          , "  comments, or explanations outside it."
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
