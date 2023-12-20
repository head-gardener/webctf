module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sqlite
import Entities
import Yesod
import Foundation
import Application ()
import System.Exit (exitFailure)
import System.Environment (getArgs)
import Yesod.Static (static, staticDevel)
import Yesod.Auth.Util.PasswordStore (makePassword)
import Data.Text.Encoding (decodeUtf8)

main :: IO ()
main = do
  port <- getArgs >>= parseArgs
  taskStat <- static "/tmp/ctf"
  stat <- staticDevel "./static"
  passwd <- makePassword "1234" 17
  let admin = User "admin" (Just $ decodeUtf8 passwd) 0
  runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    -- _ <- runResourceT $ flip runSqlPool pool $ insertBy admin
    warp port $ WebCTF pool stat taskStat
  where
    parseArgs [port] = return $ read port
    parseArgs [] = return 3000
    parseArgs _ = putStrLn "unexpected extra args" >> exitFailure
