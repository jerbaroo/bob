{-# OPTIONS_GHC -Wno-orphans #-}

module Agent.Bob.Data.Event where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Agent.Bob.Data.Task (Task)
import Agent.Bob.Data.Message (Message)
import Witch (From(..), TryFrom(..), maybeTryFrom)

data Event = forall e. (Aeson.ToJSON e, IsEvent e) => Event e

instance (Aeson.ToJSON e, IsEvent e) => From e Event where
  from = Event

instance IsEvent Event where
  eventName (Event e) = eventName e
  eventTasks (Event e) = eventTasks e
  eventToMessage (Event e) = eventToMessage e

instance Aeson.ToJSON Event where
  toJSON (Event e) = Aeson.object
    [ ("name", Aeson.toJSON $ eventName e)
    , ("value", Aeson.toJSON e)
    ]

-- | Something that has happened.
class IsEvent e where
  -- TODO avoid parameter 'e'.
  eventName :: e -> Text -- TODO use Typeable?
  eventTasks :: e -> Maybe [Task]
  eventTasks _ = Nothing
  eventToMessage :: e -> Maybe Message
  eventToMessage _ = Nothing

instance IsEvent e => TryFrom e Message where
  tryFrom = maybeTryFrom eventToMessage

instance IsEvent e => TryFrom e [Task] where
  tryFrom = maybeTryFrom eventTasks
