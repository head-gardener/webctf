module Entities where

import Data.Text
import Database.Persist.TH
import Data.Typeable
import Yesod.Auth.HashDB

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
  name Text
  password Text Maybe
  pts Int
  solved [Int]
  UniqueUser name
  deriving Typeable
|]

instance HashDBUser User where
  userPasswordHash = userPassword
  setPasswordHash h u = u { userPassword = Just h }

defaultUser :: Text -> User
defaultUser n = User n Nothing 0 []
