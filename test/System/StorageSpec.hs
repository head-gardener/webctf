module System.StorageSpec where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.Time
import System.Directory (doesPathExist)
import System.FilePath
import System.Storage as ST
import System.Storage.Memory
import System.Storage.Native
import Test.Hspec
-- import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "storage" $ do
    it "stores files" $ do
        memoryShouldReturn (Right "test") $ do
          written <- ST.writeFile "testFile" "test"
          either (return . Left) (const $ ST.readFile "testFile") written
        nativeShouldReturn (Right "test") $ do
          written <- ST.writeFile "testFile" "test"
          either (return . Left) (const $ ST.readFile "testFile") written

    it "fails on invalid reads" $ do
        memoryShouldReturn (Left NotFound) (ST.readFile "testFile")
        nativeShouldReturn (Left NotFound) (ST.readFile "testFile")

    describe "native" $ do
      it "doesn't allow directory traversal" $ do
        nativeShouldReturn' (Left NotFound) $ \r -> do
          let path = "./../../package.yaml"
          tgtExists <- liftIO $ doesPathExist (r </> path)
          unless tgtExists $ error "Test path doesn't exist!"
          ST.readFile path

memoryShouldReturn :: (Show a, Eq a) => a -> Memory a -> IO ()
memoryShouldReturn r a = do
  res <- liftIO $ runMemoryStorage a
  res `shouldBe` r

nativeShouldReturn' :: (Show a, Eq a) => a -> (FilePath -> Native a) -> IO ()
nativeShouldReturn' r a = do
  root <- makeTestPath
  res <- runNativeStorage (a root) root
  res `shouldBe` r

nativeShouldReturn :: (Show a, Eq a) => a -> Native a -> IO ()
nativeShouldReturn r a = nativeShouldReturn' r (const a)

makeTestPath :: IO FilePath
makeTestPath = fmap format getCurrentTime
  where
    format = ("./test-files/" ++) . intercalate "=" . words . show
