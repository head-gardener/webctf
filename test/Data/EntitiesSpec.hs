module Data.EntitiesSpec where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Entities
import Data.Maybe (isJust)
import Data.String ()
import Database.Persist.Sqlite
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "entities" $ do
    it "can be stored" $ do
      res <- runDB $ do
        key <- newEntry $ Verse "hey" "" Nothing
        get key
      fmap title res `shouldBe` Just "hey"

    describe "verses" $ do
      it "can be chain updated" $ do
        (updated0, updated1, v1, v2, f1, f2) <- runDB $ do
          f1 <- newEntry $ File "file1" "1" ""
          Just v0 <- get =<< newEntry (Verse "verse1" "" Nothing)
          update0 <- verseChainUpdate v0
          Just v1 <- get =<< newEntry (Verse "verse1" "" (Just f1))
          update1 <- verseChainUpdate v1

          Just f1' <- get f1
          f2 <- editEntry (versionRoot f1') $ File "file2" "2" ""
          Just update2 <- verseChainUpdate v1
          Just v2 <- get =<< update2
          return (isJust update0, isJust update1, verseFile v1, verseFile v2, f1, f2)
        updated0 `shouldBe` False
        updated1 `shouldBe` False
        v1 `shouldBe` Just f1
        v2 `shouldBe` Just f2

    describe "pages" $ do
      it "cab be chain updated" $ do
        (update0, update1, vs1, vs2, p1, p2) <- runDB $ do
          v11 <- newEntry $ Verse "verse1" "1" Nothing
          v21 <- newEntry $ Verse "verse2" "1" Nothing
          Just p0 <- get =<< newEntry (Page "verse1" [])
          update0 <- pageChainUpdate p0
          Just p1 <- get =<< newEntry (Page "verse1" [v11, v21])
          update1 <- pageChainUpdate p1

          Just v11' <- get v11
          Just v21' <- get v21
          v12 <- editEntry (versionRoot v11') $ Verse "verse1" "2" Nothing
          v22 <- editEntry (versionRoot v21') $ Verse "verse2" "2" Nothing
          Just update2 <- pageChainUpdate p1
          Just p2 <- get =<< update2
          return
            ( isJust update0,
              isJust update1,
              [v11, v21],
              [v12, v22],
              pageVerses p1,
              pageVerses p2
            )
        update0 `shouldBe` False
        update1 `shouldBe` False
        vs1 `shouldBe` p1
        vs2 `shouldBe` p2

runDB :: ReaderT SqlBackend (ResourceT (LoggingT IO)) a -> IO a
runDB a =
  runStderrLoggingT $
    filterLogger (\_ l -> l > LevelError) $
      withSqlitePool ":memory:" 10 $
        runResourceT . runSqlPool (runMigrationQuiet migrateAll >> a)
