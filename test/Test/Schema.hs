module Test.Schema (spec) where

import Agent.Bob.Data.AgentOutput (AgentMessageContent)
import Agent.Bob.Prelude
import Agent.Bob.Prompt.Build (responseSchema)
import Control.Monad (replicateM)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Text qualified as Text
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck qualified as H
import Test.QuickCheck (Gen, choose, counterexample, elements, forAll, property)

-- | Contract test for the response 'responseSchema': every value the schema
-- permits must be accepted by the parser.
spec :: Spec
spec =
  H.prop "responseSchema: generated values parse as AgentMessageContent" $
    forAll (genFromSchema responseSchema) $ \value ->
      case Aeson.fromJSON @AgentMessageContent value of
        Aeson.Success _ -> property True
        Aeson.Error err -> counterexample
          (err <> "\nfor value: " <> show value)
          (property False)

-- * Generate JSON from schema.

-- | Generate an array described in the "items" key of an 'Aeson.Object'.
genArray :: Aeson.Object -> Gen Aeson.Value
genArray o = case KM.lookup "items" o of
  Just items -> do
    n <- choose (0, 3 :: Int)
    Aeson.toJSON <$> replicateM n (genFromSchema items)
  Nothing -> error "genArray: array schema without items"

-- | Generate a JSON value conforming to a given schema.
genFromSchema :: Aeson.Value -> Gen Aeson.Value
genFromSchema (Aeson.Object o)
  | Just (Aeson.Array alts) <- KM.lookup "oneOf" o =
      genFromSchema =<< elements (toList alts)
  | Just constant <- KM.lookup "const" o = pure constant
  | Just (Aeson.Array vals) <- KM.lookup "enum" o = elements $ toList vals
  | Just (Aeson.String ty) <- KM.lookup "type" o = case ty of
      "object" -> genObject o
      "array"  -> genArray o
      "string" -> Aeson.String . Text.pack <$> arbitrary
      other    -> error $ "genFromSchema: unsupported type " <> Text.unpack other
genFromSchema s = error $ "genFromSchema: unsupported schema " <> show s

-- | Generate a JSON value described by optional and required properties.
genObject :: Aeson.Object -> Gen Aeson.Value
genObject o = do
  let props = case KM.lookup "properties" o of
        Just (Aeson.Object p) -> KM.toList p
        _                     -> []
      required = case KM.lookup "required" o of
        Just (Aeson.Array r) -> [ k | Aeson.String k <- toList r ]
        _                    -> []
  fields <- forM props $ \(key, sub) -> do
    -- Required properties always appear; optional ones appear at random.
    include <- if Key.toText key `elem` required then pure True else arbitrary
    if include then (\v -> Just (key, v)) <$> genFromSchema sub else pure Nothing
  pure . Aeson.Object . KM.fromList $ catMaybes fields
