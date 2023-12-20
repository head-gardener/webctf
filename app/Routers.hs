module Routers where

import Control.Monad (when)
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
    mapRoute "test" = TaskStaticR test
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
      <$> areq textField "Login" Nothing
      <*> areq textField "Password" Nothing

getTasksR :: Handler Html
getTasksR = do
  uid <- maybeAuthId
  solved <- case uid of
    Nothing -> return []
    Just u -> do
      u' <- runDB $ get404 u
      return $ userSolved u'
  -- solved <- maybe (return []) (userSolved <$> (runDB . get404)) uid
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
  defPage ("Task " <> show n) $ do
    t <- tasks !? (n - 1)
    _ <- maybe (redirect $ AuthR LoginR) return =<< maybeAuthId
    [whamlet|
      <h3>Descriprion:
      $case (input t)
        $of TextInput i
          <p>#{i}
        $of LinkInput (l, i)
          <p><a href=@{l}>#{i} 
      <form method="post" action=@{TaskR n} enctype=#{enctype}>
        ^{widget}
        <button>Submit
    |]

formresult :: (FormResult t1 -> t2) -> (t1 -> t2) -> FormResult t1 -> t2
formresult _ f (FormSuccess x) = f x
formresult f _ r = f r

postTaskR :: Int -> Handler Html
postTaskR n = do
  uid <- maybe (redirect $ AuthR LoginR) return =<< maybeAuthId
  ((result, _), _) <- runFormPost taskForm
  f <- formresult (const $ onFail "Incorrect input") return result
  t <- tasks !? (n - 1)
  when (flag t /= f) $ onFail "Incorrect answer"
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
      <li> head-gardener (tech-lead)
      <li> fisherman (design/frontend)
      <li> bold dude (technical advisor)
    |]

getLoginR :: Handler Html
getLoginR = defPage "Login" $ do
  [whamlet|<p>Login here|]

-- defaultPage ::
--   ( PersistEntityBackend a ~ SqlBackend,
--     PersistEntity a,
--     HasVersions a,
--     Drawable a
--   ) =>
--   Key a ->
--   HandlerFor BookShelf Html
-- defaultPage objId = do
--   obj <- runDB (get404 objId)
--   versions <- runDB $ allVersions obj
--   defaultLayout $ do
--     draw Timestamped obj
--     [whamlet|<h3>History:|]
--     mapM_ reference versions

-- getVerseR :: VerseId -> Handler Html
-- getVerseR = defaultPage

-- getPageR :: PageId -> Handler Html
-- getPageR = defaultPage

-- getFileR :: FileId -> Handler Html
-- getFileR = defaultPage

-- getStorageR :: Text -> Handler Html
-- getStorageR fileName =
--   runDB (getBy405 $ UniqueFName fileName) >>= sendF . entityVal
--   where
--     sendF f = do
--       path <-
--         either (const notFound) return
--           =<< runStorage (ST.exportPath $ T.unpack $ ES.fileName f)
--       sendFile (fileType f) path
