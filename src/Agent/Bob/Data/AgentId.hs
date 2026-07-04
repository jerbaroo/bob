module Agent.Bob.Data.AgentId where

import Witch (From(..))
import Agent.Bob.Pretty (Pretty(..))

newtype AgentId = AgentId Int
  deriving (Eq, Ord, Show)

instance Pretty AgentId where
  pretty (AgentId n) = "Agent #" <> from (show n)
