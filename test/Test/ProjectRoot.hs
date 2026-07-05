module Test.ProjectRoot (spec) where

import Agent.Bob.Prelude
import Agent.Bob.ProjectRoot (findProjectRootBy, keyFromHead, parseHead)
import Data.Functor.Identity (Identity(..))
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Run the walk purely: the given directories are the ones containing .git.
walk :: [FilePath] -> FilePath -> Maybe FilePath
walk roots = runIdentity . findProjectRootBy (Identity . (`elem` roots))

spec :: Spec
spec = describe "findProjectRootBy" $ do
  it "finds the root when starting inside it" $
    walk ["/home/u/repo"] "/home/u/repo/src/deep" `shouldBe` Just "/home/u/repo"

  it "finds the root when starting at it" $
    walk ["/home/u/repo"] "/home/u/repo" `shouldBe` Just "/home/u/repo"

  it "prefers the nearest root (nested repos)" $
    walk ["/home/u/repo", "/home/u/repo/vendored"] "/home/u/repo/vendored/x"
      `shouldBe` Just "/home/u/repo/vendored"

  it "returns Nothing when no ancestor is a root" $
    walk ["/home/u/repo"] "/tmp/elsewhere" `shouldBe` Nothing

  it "terminates at the filesystem root" $
    walk [] "/" `shouldBe` Nothing

  describe "parseHead" $ do
    it "parses a symbolic ref to the branch name" $
      parseHead "ref: refs/heads/main\n" `shouldBe` Just "main"

    it "keeps slashes in branch names" $
      parseHead "ref: refs/heads/feature/foo\n" `shouldBe` Just "feature/foo"

    it "parses a detached HEAD to the short SHA" $
      parseHead "0123456789abcdef0123456789abcdef01234567\n"
        `shouldBe` Just "0123456"

    it "rejects junk" $
      parseHead "\n" `shouldBe` Nothing

  describe "keyFromHead" $ do
    it "sanitises slashes for use as a filename" $
      keyFromHead (Just "feature/foo") `shouldBe` "feature-foo"

    it "falls back to default outside a repository" $
      keyFromHead Nothing `shouldBe` "default"
