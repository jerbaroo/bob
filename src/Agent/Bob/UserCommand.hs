module Agent.Bob.UserCommand where

import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Prelude
import Data.Text qualified as Text

-- | A command typed by the user e.g. @/config show@.
class IsUserCommand c where
  -- | Parse the words following 'userCommandName' into this command.
  mkUserCommand :: [Text] -> Maybe c
  mkUserCommand _ = Nothing
  userCommandDescription :: Text
  -- | The words that invoke this command, e.g. @["/config", "show"]@.
  userCommandName :: [Text]
  -- | Attempt to parse user input into this command: 'userCommandName'
  -- followed by words that 'mkUserCommand' accepts.
  parseUserCommand :: UserInput -> Maybe c
  parseUserCommand input =
    let expectedName = userCommandName @c
        inputWords = Text.words $ Text.strip $ from input
    in  if   expectedName `isPrefixOf` inputWords
        then mkUserCommand $ drop (length expectedName) inputWords
        else Nothing
