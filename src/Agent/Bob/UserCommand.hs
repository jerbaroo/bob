module Agent.Bob.UserCommand where

import Agent.Bob.Command (IsCommand)
import Agent.Bob.Data.Event (IsEvent)
import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

-- | An existential over a user command value. The constraints let 'run'
-- execute it directly with 'Agent.Bob.Command.step'.
data UserCommand
  = forall c
  . (Aeson.ToJSON c, IsCommand c, IsEvent c, IsUserCommand c, Typeable c)
  => UserCommand c

instance Aeson.ToJSON UserCommand where
  toJSON (UserCommand c) = Aeson.toJSON c

-- | A command typed by the user e.g. @/config show@.
class IsUserCommand c where
  -- | Parse the words following 'userCommandName' into this command.
  mkUserCommand :: [Text] -> Maybe c
  mkUserCommand _ = Nothing
  -- | Attempt to parse user input into this command: 'userCommandName'
  -- followed by words that 'mkUserCommand' accepts.
  parseUserCommand :: UserInput -> Maybe c
  parseUserCommand input =
    let expectedName = userCommandName @c
        inputWords = Text.words $ Text.strip $ from input
    in  if   expectedName `isPrefixOf` inputWords
        then mkUserCommand $ drop (length expectedName) inputWords
        else Nothing
  userCommandDescription :: Text
  -- | The words that invoke this command, e.g. @["/config", "show"]@.
  userCommandName :: [Text]
