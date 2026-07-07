module Agent.Bob.Effect.Permission where

import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.Permission (Decision(..), parseDecision)
import Agent.Bob.Data.HarnessState (HarnessState)
import Agent.Bob.Data.ToolName (ToolName)
import Agent.Bob.Effect.Console (Console, readUserInput, writeConsole)
import Agent.Bob.Effect.File (File, appendFile)
import Agent.Bob.Prelude hiding (appendFile)

data Permission :: Effect where
  -- | Ask whether the tool may run with the given parameter.
  RequestPermission :: ToolName -> Text -> Permission f Decision
makeEffectF ''Permission

-- | Ask the user for permission on the console.
runPermissionAsk
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => Eff (Permission : es) ~> Eff es
runPermissionAsk = interpret \case
  RequestPermission name param -> do
    let grant = into @Text name <> " " <> param
    writeConsole $ "\nBob wants permission: " <> grant
      <> "\nAllow? (y)es once / (a)lways / anything else denies\n"
    decision <- parseDecision . from <$> readUserInput
    when (decision == AllowAlways) do
      path <- asks (.permissionsPath)
      appendFile path $ grant <> "\n"
    pure decision

-- | Deny everything; for non-interactive runs (@--no-ask@).
runPermissionDeny :: Eff (Permission : es) ~> Eff es
runPermissionDeny = interpret \case
  RequestPermission _ _ -> pure Deny
