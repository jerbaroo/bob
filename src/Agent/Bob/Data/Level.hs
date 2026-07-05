module Agent.Bob.Data.Level where

import Agent.Bob.Prelude

import Agent.Bob.Pretty (Pretty(..))

data Level
  = Info
  | Thinking
  | Meta
  | Debug
  deriving (Eq, Ord)

instance Pretty Level where
  pretty Debug = "debug"
  pretty Info = "info"
  pretty Meta = "meta"
  pretty Thinking = "thinking"
