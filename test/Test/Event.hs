module Test.Event (spec) where

import Agent.Bob.Data.Event (Event(..), IsEvent)
import Agent.Bob.Data.Event.Registry (LineResult(..), SomeEvent(..), knownEvents, readEventLine)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (compact)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck qualified as H

spec :: Spec
spec = do
  mapM_ eventPropTests knownEvents
  classifySpec

-- | 'readEventLine' must separate genuine corruption from lines that are simply
-- not restored, so the restore step only reports the former.
classifySpec :: Spec
classifySpec = describe "readEventLine" $ do
  it "restores a known event type" $
    tag (readEventLine "{\"name\":\"UserInput\",\"value\":\"hi\"}")
      `shouldBe` "restored"
  it "skips a well-formed but non-restored type (e.g. a command)" $
    tag (readEventLine "{\"name\":\"Command:Help\",\"value\":\"Help\"}")
      `shouldBe` "skipped"
  it "flags a non-JSON line as malformed" $
    tag (readEventLine "GARBAGE-NOT-JSON") `shouldBe` "malformed"
  it "flags a known type with an undecodable value as malformed" $
    tag (readEventLine "{\"name\":\"UserInput\",\"value\":{}}")
      `shouldBe` "malformed"
 where
  tag :: LineResult -> Text
  tag = \case
    Restored _ -> "restored"
    Skipped    -> "skipped"
    Malformed  -> "malformed"

eventPropTests :: SomeEvent -> Spec
eventPropTests (SomeEvent (_ :: Proxy e)) = do
  jsonRoundtripPropTest @e
  eventLineRoundtripPropTest @e

jsonRoundtripPropTest
  :: forall a
   . (Aeson.FromJSON a, Aeson.ToJSON a, Arbitrary a, Eq a, Show a, Typeable a)
  => Spec
jsonRoundtripPropTest = H.prop
  ("JSON roundtrip prop test: " <> Text.unpack (showTyCon @a))
  $ \(a :: a) -> Aeson.eitherDecode (Aeson.encode a) `shouldBe` Right a

-- | The full loop through the session log: serialize as a log line, restore
-- via the registry, and compare as JSON (raw 'Event's have no 'Eq').
eventLineRoundtripPropTest
  :: forall e
   . (Aeson.ToJSON e, Arbitrary e, IsEvent e, Show e, Typeable e)
  => Spec
eventLineRoundtripPropTest = H.prop
  ("Session-log line roundtrip prop test: " <> Text.unpack (showTyCon @e))
  $ \(e :: e) ->
      let event = Event e
      in  Aeson.toJSON <$> into @(Maybe Event) (readEventLine $ compact event)
          `shouldBe` Just (Aeson.toJSON event)
