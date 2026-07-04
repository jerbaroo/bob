module Agent.Bob.UserCommand where

import Agent.Bob.Data.UserInput (UserInput)

-- | A command typed by the user (e.g. @/config show@)
class IsUserCommand c where
  -- | Attempt to parse user input into this command.
  parseUserCommand :: UserInput -> Maybe c
