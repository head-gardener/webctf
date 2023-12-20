module Render where

-- import Data.ContentType as CT
-- import Data.Entities as ES
-- import Data.Text qualified as T
-- import Foundation
-- import Yesod
-- import System.Storage qualified as ST
-- import Data.ByteString.Lazy.Char8 qualified as BL

-- data Format = Timestamped | Minimal
--   deriving (Show, Eq)

-- class (DBEntity a) => Drawable a where
--   route :: Entity a -> Route BookShelf
--   drawContent :: a -> Widget
--   drawSummary :: a -> Widget
--   drawSummary = drawContent

-- draw :: Drawable a => Format -> a -> Widget
-- draw f a =
--   [whamlet|
--     <h3>#{title a}
--     $if f == Timestamped
--       <p>#{show $ time a}
--     ^{drawContent a}
--   |]

-- summary :: Drawable a => a -> Widget
-- summary a =
--   [whamlet|
--     <h3>#{title a}
--     ^{drawSummary a}
--   |]

-- reference :: Drawable a => Entity a -> Widget
-- reference a =
--   [whamlet|
--     <h3>
--       <a href=@{route a}>#{title $ entityVal a}
--     ^{drawSummary $ entityVal a}
--   |]

-- instance Drawable Verse where
--   route = VerseR . entityKey
--   drawContent v = do
--     -- TODO: this sucks
--     file <- case verseFile v of
--       Just f -> liftHandler $ runDB $ get f
--       Nothing -> return Nothing
--     [whamlet|
--       <p>#{verseContent v}
--       ^{mapM_ drawContent file}
--     |]
--   drawSummary v =
--     [whamlet|
--     <p>#{T.append (T.take 25 $ verseContent v) "..."}
--   |]

-- instance Drawable Page where
--   route = PageR . entityKey
--   drawContent p = do
--     verses <- liftHandler $ mapM (runDB . get404) $ pageVerses p
--     [whamlet|^{mapM_ (draw Minimal) verses}|]
--   drawSummary p = do
--     verses <- liftHandler $ mapM (runDB . get404) $ pageVerses p
--     [whamlet|
--       $forall v <- verses
--         <p>#{title v}
--     |]

-- instance Drawable File where
--   route = FileR . entityKey
--   drawContent f = do
--     case fileType f of
--       "text/plain" -> do
--         c <- runStorage $ ST.readFile $ T.unpack $ ES.fileName f
--         s <- either (const notFound) (return . BL.unpack) c
--         toWidget
--           [hamlet|
--           <p>Preview:
--           <p>#{take 100 s}
--         |]
--       "image/png" -> do
--         toWidget
--           [hamlet|
--           <img
--             src=@{StorageR $ ES.fileName f}
--             alt=#{fileTitle f}
--             width=300 height=300>
--         |]
--       _ -> toWidget [hamlet|<p>can't preview|]
--     toWidget [hamlet|<p><a href=@{StorageR $ ES.fileName f}>Download|]
--   drawSummary f = toWidget [hamlet|<p>#{CT.unpack $ fileType f}|]
