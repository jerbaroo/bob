module Agent.Bob.Prompt.Build where

import Agent.Bob.Data.AgentInput (AgentInput(..))
import Agent.Bob.Data.Event (eventToMessage)
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Prelude
import Agent.Bob.Prompt (ToPrompt(..))
import Agent.Bob.Tool.Registry (toolCallSchema, toolDescriptions)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

buildPrompt :: AgentInput -> Aeson.Value
buildPrompt AgentInput{..} =
  Aeson.object
    [ ("messages", Aeson.toJSON $ systemMessage : eventMessages)
    , ("model", Aeson.toJSON modelName)
    , ("response_format", Aeson.object
        [ ("type", "json_schema")
        , ("schema", responseSchema)
        ])
    , ("temperature", Aeson.toJSON modelTemperature)
    ]
 where
  -- Events are stored newest-first; reverse into chronological order. The
  -- current user input and any tool results are among them ('UserInput' and
  -- 'ToolResult' are recorded before the agent is called), so no separate
  -- messages are added.
  eventMessages :: [Message]
  eventMessages = reverse . catMaybes $ eventToMessage <$> events

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
          [ "### RESPONSE FORMAT"
          , "Your response is a single JSON object (its shape is enforced for you). Fill"
          , "in its keys as follows:"
          , "- \"message\" (required): a string containing your conversational reply to the"
          , "  user. Markdown IS allowed inside this string (code fences, lists, emphasis)."
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
          ]
        availableToolsStr = Text.intercalate "\n"
          $ "### AVAILABLE TOOLS" : toolDescriptions
        availableSkillsStr = Text.intercalate "\n" $ "### AVAILABLE SKILLS" :
          case modelSkills of
            [] -> ["None."]
            _  ->
              [ "To use a skill, read its file with the read_files tool."
              , toPrompt modelSkills
              ]
        content = Text.intercalate "\n\n"
          [ personaStr
          , modelSoul
          , tasksStr modelTasks
          , responseFormatStr
          , availableToolsStr
          , availableSkillsStr
          ]
    in  Message {role=System, thinking=Nothing, ..}

-- | JSON Schema for the model's response.
responseSchema :: Aeson.Value
responseSchema = Aeson.object
  [ ("type", "object")
  , ("additionalProperties", Aeson.Bool False)
  , ("required", Aeson.toJSON ["message", "remaining_tasks" :: Text])
  , ("properties", Aeson.object
      [ ("message", Aeson.object [("type", "string")])
      , ("remaining_tasks", Aeson.object
          [ ("type", "array")
          , ("items", Aeson.object [("type", "string")])
          ])
      , ("tool_calls", Aeson.object
          [ ("type", "array")
          , ("items", toolCallSchema)
          ])
      ])
  ]
