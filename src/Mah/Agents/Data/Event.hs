{-# OPTIONS_GHC -Wno-orphans #-}

module Mah.Agents.Data.Event where

import Mah.Agents.Data.Task (Task)
import Mah.Agents.Data.Message (Message)
import Mah.Agents.Pretty (Pretty(..))
import Witch (From(..), TryFrom(..), maybeTryFrom)

data Event = forall e. (IsEvent e, Pretty e) => Event e

instance (IsEvent e, Pretty e) => From e Event where
  from = Event

instance IsEvent Event where
  eventTasks (Event e) = eventTasks e
  eventToMessage (Event e) = eventToMessage e

instance Pretty Event where
  pretty (Event e) = pretty e

-- | Something that has happened.
class IsEvent e where
  eventTasks :: e -> Maybe [Task]
  eventTasks _ = Nothing
  eventToMessage :: e -> Maybe Message
  eventToMessage _ = Nothing

instance IsEvent e => TryFrom e Message where
  tryFrom = maybeTryFrom eventToMessage

instance IsEvent e => TryFrom e [Task] where
  tryFrom = maybeTryFrom eventTasks
