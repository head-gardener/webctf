module Data.GitignoreSpec where

import Data.Gitignore
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "gitignore parser" $ do
    it "handles trivial cases" $ do
      applyGitignore [] ["file1", "file2"] `shouldBe` ["file1", "file2"]

    it "handles flat directories" $ do
      applyGitignore ["file1"] ["file1", "file2"] `shouldBe` ["file2"]
      applyGitignore ["file*"] ["file1", "file2", "ff"] `shouldBe` ["ff"]
      applyGitignore ["*File"] ["1File", "2File", "ff"] `shouldBe` ["ff"]
      applyGitignore ["*il*"] ["File", "file", "ff"] `shouldBe` ["ff"]

    it "handles trees" $ do
      applyGitignore ["dir"] ["dir", "dir/file", "file"] `shouldBe` ["file"]
      applyGitignore ["di*"] ["dir", "dir/file", "ff"] `shouldBe` ["ff"]
      applyGitignore ["**/file"] ["dir", "dir/file", "ff"] `shouldBe` ["dir", "ff"]

    it "ignores comments" $ do
      applyGitignore ["#*"] ["#-file"] `shouldBe` ["#-file"]
