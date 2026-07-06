module Agent.Bob.Data.Event where

import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Agent.Bob.Data.Task (Task)
import Agent.Bob.Data.Message (Message)

-- TODO should our class constraints be done a different way?

data Event = forall e. (Aeson.ToJSON e, IsEvent e, Typeable e) => Event e

instance (Aeson.ToJSON e, IsEvent e, Typeable e) => From e Event where
  from = Event

instance IsEvent Event where
  eventTasks (Event e) = eventTasks e
  eventToMessage (Event e) = eventToMessage e

instance Aeson.ToJSON Event where
  toJSON (Event (e :: e)) = Aeson.object
    [ ("name", Aeson.toJSON $ eventName @e)
    , ("value", Aeson.toJSON e)
    ]

-- | Something that has happened.
class IsEvent e where
  -- | Updated list of tasks for the harness state.
  -- TODO should this be here?
  eventTasks :: e -> Maybe [Task]
  -- | How the event appears in the model's context; no default so a new
  -- event type must decide explicitly whether the model sees it.
  -- TODO should this be a separate typeclass?
  eventToMessage :: e -> Maybe Message

eventName :: forall t. Typeable t => Text
eventName = from $ showTyCon @t

eventName' :: Event -> Text
eventName' (Event (_ :: t)) = eventName @t
