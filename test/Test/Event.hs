module Test.Event (spec) where

import Agent.Bob.Data.Event (Event(..), IsEvent)
import Agent.Bob.Data.Event.Registry (SomeEvent(..), knownEvents, parseEventLine)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (compact)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Test.Hspec (Spec, shouldBe)
import Test.Hspec.QuickCheck qualified as H

spec :: Spec
spec = mapM_ eventPropTests knownEvents

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
      in  (Aeson.toJSON <$> parseEventLine (compact event))
            `shouldBe` Just (Aeson.toJSON event)
