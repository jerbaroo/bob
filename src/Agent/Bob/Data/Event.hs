{-# OPTIONS_GHC -Wno-orphans #-}

module Agent.Bob.Data.Event where

import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import Agent.Bob.Data.Task (Task)
import Agent.Bob.Data.Message (Message)
import Witch (From(..))

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
  eventName :: e -> Text
  default eventName :: Typeable e => e -> Text
  eventName _ = from $ show $ typeRep (Proxy @e)
  -- | Updated list of tasks for the harness state.
  eventTasks :: e -> Maybe [Task]
  -- | How the event appears in the model's context; no default so a new
  -- event type must decide explicitly whether the model sees it.
  eventToMessage :: e -> Maybe Message
