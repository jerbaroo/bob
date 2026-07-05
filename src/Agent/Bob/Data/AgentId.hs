module Agent.Bob.Data.AgentId where

import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))

newtype AgentId = AgentId Int
  deriving (Eq, Ord, Show)

instance Pretty AgentId where
  pretty (AgentId n) = "Agent #" <> from (show n)
