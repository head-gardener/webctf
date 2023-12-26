module Foundation where

import Control.Monad (when)
import Database.Persist.Sqlite
import Entities
import Yesod
import Yesod.Auth
import Yesod.Auth.HashDB (authHashDB)
import Yesod.Static

data WebCTF = WebCTF
  { pool :: ConnectionPool,
    appStatic :: Static,
    taskFiles :: Static
  }

staticFiles "./static"
staticFiles "./common"

mkYesodData "WebCTF" $(parseRoutesFile "app/routes")

header :: Widget
header = do
  maid <- maybeAuthId
  toWidget
    [hamlet|
      <div .navBar>    
        <h3>
          WebCTF - #
          <a href=@{TasksR}>TASKS
          <a href=@{RulesR}>RULES
          <a href=@{LBoardR}>LEADERBOARD
          <a href=@{AboutR}>ABOUT
          $case maid
            $of Just _
              <a href=@{AuthR LogoutR}>LOGOUT
            $of Nothing
              <a href=@{AuthR LoginR}>LOGIN
    |]

footer :: Widget
footer = [whamlet|
    <p .footer>
      <a .footerlink href="https://4anime.is/serial-experiments-lain-503">
        head-gardener, 2023. Keep your head up.
  |] 

defPage :: (MonadHandler m, m ~ HandlerFor WebCTF) => String -> Widget -> m Html
defPage = defPage' True

defPage' :: (MonadHandler m, m ~ HandlerFor WebCTF) => Bool -> String -> Widget -> m Html
defPage' h s w = defaultLayout $ do
  setTitle $ toHtml s
  header
  addStylesheet $ StaticR styles_css
  when h [whamlet|<h1>#{s}|]
  w
  footer

instance YesodPersist WebCTF where
  type YesodPersistBackend WebCTF = SqlBackend
  runDB action = runSqlPool action =<< getsYesod pool

instance RenderMessage WebCTF FormMessage where
  renderMessage _ _ = defaultFormMessage

instance Yesod WebCTF where
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

instance YesodAuth WebCTF where
  type AuthId WebCTF = UserId

  loginDest _ = TasksR
  logoutDest _ = TasksR

  authPlugins _ = [authHashDB (Just . UniqueUser)]

  authenticate creds = liftHandler $ runDB $ do
    x <- insertBy $ defaultUser $ credsIdent creds
    return $
      Authenticated $
        case x of
          Left (Entity userid _) -> userid
          Right userid -> userid

  authLayout =
    liftHandler
      . defPage' False "Authenticate"
      . ( >>
            [whamlet|<p>
    <a href=@{RegisterR}>Register|]
        )

instance YesodAuthPersist WebCTF
