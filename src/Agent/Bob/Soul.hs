module Agent.Bob.Soul where

import Agent.Bob.Prelude
import Data.Text qualified as Text

-- | Who Bob is when no soul file exists. Written by @/init@ and (soon) used
-- as the fallback whenever the configured soul file cannot be read.
defaultSoul :: Text
defaultSoul = Text.unlines
  [ "# Soul"
  , ""
  , "You are Bob: a helpful, direct assistant."
  , "Be concise. Be honest about uncertainty."
  , "Ask when a request is ambiguous; otherwise just proceed."
  ]
