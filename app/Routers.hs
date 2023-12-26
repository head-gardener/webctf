module Routers where

import Control.Monad (when)
import Data.Char (isAlphaNum, isSpace)
import Data.Task
import Data.Text (Text)
import Data.Text qualified as T
import Entities
import Foundation
import Yesod
import Yesod.Auth
import Yesod.Auth.HashDB (setPassword)

tasks :: [Task (Route WebCTF, Text)]
tasks = mkTasks $ \a -> (mapRoute a, a)
  where
    mapRoute :: Text -> Route WebCTF
    mapRoute "morse.txt" = TaskStaticR morse_txt
    mapRoute "secret.pcapng" = TaskStaticR secret_pcapng
    mapRoute "photo1.jpg" = TaskStaticR photo1_jpg
    mapRoute "comms.zip" = TaskStaticR comms_zip
    mapRoute "keychain.txt" = TaskStaticR keychain_txt
    mapRoute "db.sqlite3" = TaskStaticR db_sqlite3
    mapRoute "log.txt" = TaskStaticR log_txt
    mapRoute "secret.png" = TaskStaticR secret_png
    mapRoute "charade.jpg" = TaskStaticR charade_jpg
    mapRoute r = error $ "Unexpected test file " <> T.unpack r

getRootR :: Handler Html
getRootR = redirect TasksR

getRegisterR :: Handler Html
getRegisterR = do
  (widget, enctype) <- generateFormPost registerForm
  defPage
    "Register"
    [whamlet|
    <form method="post" action=@{RegisterR} enctype=#{enctype}>
      ^{widget}
      <button>Register
  |]

postRegisterR :: Handler Html
postRegisterR = do
  ((result, _), _) <- runFormPost registerForm
  case result of
    FormSuccess u -> do
      user <- setPassword (snd u) $ defaultUser $ fst u
      add <- runDB $ insertBy user
      either (const onFail) (const $ redirect TasksR) add
    _ -> onFail
  where
    onFail = do
      setMessage $ toHtml ("Invalid credentials" :: Text)
      redirect RegisterR

registerForm :: Html -> MForm Handler (FormResult (Text, Text), Widget)
registerForm =
  renderDivs $
    (,)
      <$> areq nickField "Login" Nothing
      <*> areq textField "Password" Nothing
  where
    nickError :: Text
    nickError = "Only alphanum and spaces allowed for nick."

    nickField = check validateNick textField

    validateNick :: Text -> Either Text Text
    validateNick s = if isAlphaNumOrSpace s then Right s else Left nickError

    isAlphaNumOrSpace :: Text -> Bool
    isAlphaNumOrSpace = all (\c -> isAlphaNum c || isSpace c) . T.unpack

getTasksR :: Handler Html
getTasksR = do
  uid <- maybeAuthId
  solved <- case uid of
    Nothing -> return []
    Just u -> do
      u' <- runDB $ get404 u
      return $ userSolved u'
  defPage "Tasks" $ do
    mapM_ (showTask solved) $ zip tasks [(1 :: Int) ..]
  where
    isSolved :: Int -> [Int] -> String
    isSolved n s = if n `elem` s then "solved" else ""

    showTask s (t, n) =
      toWidget
        [hamlet|
          <div .taskbox .#{isSolved n s}>
            <a href=@{TaskR n} .tboxlink>
            <h3>Task #{n}
            <h3>#{showPoints $ weigh t}
        |]

showPoints :: (Show a, Eq a, Num a) => a -> String
showPoints w = show w <> " point" <> if w /= 1 then "s" else ""

(!?) :: (MonadHandler m) => [a] -> Int -> m a
(!?) xs i
  | i < 0 || i >= length xs = notFound
  | otherwise = return $ xs !! i

getTaskR :: Int -> Handler Html
getTaskR n = do
  (widget, enctype) <- generateFormPost taskForm
  -- this fucking sucks
  us <-
    fmap fst
      . filter (\u -> n `elem` snd u)
      . fmap ((\u -> (userName u, userSolved u)) . entityVal)
      <$> runDB (selectList [] [])
  defPage ("Task " <> show n) $ do
    t <- tasks !? (n - 1)
    _ <- maybe (redirect $ AuthR LoginR) return =<< maybeAuthId
    inp <- case input t of
      TextInput i -> return [whamlet|<p>#{i}|]
      LinkInput (l, i) ->
        return
          [whamlet|
          <p>Download: #
            <b>
              <a href=@{l}>#{i}|]
      MonadInput i -> liftIO i >>= \x -> return [whamlet|<p>#{x}|]
    [whamlet|
      <h3>Input:
      ^{inp}
      <h3>Tip:
      <p>#{tip t}
      <h3>Submit:
      <form method="post" action=@{TaskR n} enctype=#{enctype}>
        ^{widget}
        <button>Submit
      <p>Solved by: #{showUsers us}
    |]
  where
    showUsers :: [Text] -> Text
    showUsers us =
      let (postfix, us') = if length us > 10 then ("...", take 10 us) else ("", us)
       in T.intercalate ", " us' <> postfix

formresult :: (FormResult t1 -> t2) -> (t1 -> t2) -> FormResult t1 -> t2
formresult _ f (FormSuccess x) = f x
formresult f _ r = f r

postTaskR :: Int -> Handler Html
postTaskR n = do
  uid <- maybe (redirect $ AuthR LoginR) return =<< maybeAuthId
  ((result, _), _) <- runFormPost taskForm
  f <- formresult (const $ onFail "Incorrect input") return result
  t <- tasks !? (n - 1)
  fl <- case flag t of
    TextFlag a -> return a
    MonadFlag a -> liftIO a
  when (fl /= f) $ onFail "Incorrect answer"
  user <- runDB $ get404 uid
  let solved = userSolved user
  let points = userPts user
  when (n `elem` solved) $ onFail "Correct"
  runDB $ update uid [UserPts +=. weigh t, UserSolved =. n : solved]
  setMessage $ toHtml $ "Correct! You now got " <> showPoints (points + weigh t)
  redirect TasksR
  where
    onFail :: Text -> Handler a
    onFail t = do
      setMessage $ toHtml t
      redirect $ TaskR n

taskForm :: Html -> MForm Handler (FormResult Text, Widget)
taskForm = renderDivs $ areq textField "" Nothing

getRulesR :: Handler Html
getRulesR = defPage "Rules" $ do
  toWidget
    [hamlet|
    <p>Do no evil.
    <ul>
      <li>Solve tasks, get points and ranking.
      <li>All solutions are in the form of "flag{...}", unless stated otherwise.
      <li>Misbehaviour will get you banned.
    |]

getLBoardR :: Handler Html
getLBoardR = do
  users <- fmap entityVal <$> runDB (selectList [] [Desc UserPts, LimitTo 10])
  defPage
    "Leaderboard"
    [whamlet|
      <ol>
        $forall u <- users
          <li>#{userName u}: #{length $ userSolved u} solved, #{showPoints $ userPts u}
    |]

getAboutR :: Handler Html
getAboutR = defPage "About" $ do
  toWidget
    [hamlet|
    <p>Powered by Haskell
    <p> Made By:
    <ul>
      <li> driver (team-lead)
      <li> head-gardener (junior, sysadm)
      <li> fisherman (design, PR)
      <li> bold guy (tech-advisor)
    |]
