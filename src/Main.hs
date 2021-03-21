
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveGeneric#-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import System.Directory (listDirectory)
import Data.List (isPrefixOf,map,length)
import Data.Text.Lazy (toStrict,fromStrict,unpack,pack,Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as DT
import System.FilePath.Posix ((</>))
import Text.Pandoc
import GHC.Generics
import Data.Aeson as AE
import Control.Applicative ((<$>),(<*>))
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified System.Environment as Env


data Post = Post { id :: Int
                 , title :: Text
                 , created :: Text
                 , body :: Text
                 , slug :: Text
                 } deriving (Generic,Show)

instance ToJSON Post where
instance FromJSON Post where
instance FromRow Post where
    fromRow = Post <$> field <*> field <*> field <*> field <*> field
instance ToRow Post where
    toRow (Post id_ title created body slug) = toRow (id_,title,created,body,slug)

defaultPort :: Int
defaultPort = 3001

main :: IO ()
main = do
    putStrLn "App started at 3001"
    conn <- open "blog.db"
--    result <- query_ conn "select * from content" :: IO [Post]
--    mapM_ print result
    
    configuredPort <- Env.lookupEnv "PORT"
    let port = case configuredPort of
                  Just x -> read x
                  Nothing -> defaultPort

    scotty port $ app conn
    close conn


app :: Connection -> ScottyM ()
app conn = do
    get "/" $ indexHandler conn
    get (regex "^/([a-zA-Z0-9|-]{3,})$") blogHandler
    get (regex "^/tag:([a-zA-Z0-9|-]{3,})$") tagsHandler


indexHandler :: Connection -> ActionM ()
indexHandler conn = do
    contents <- liftIO $ (query_ conn "SELECT * FROM content" :: (IO [Post]))
    text $ "Total content count is:"<> (pack (show $ length contents))

blogHandler :: ActionM ()
blogHandler = do
    slug <- param "0"
    files <- liftIO $ listDirectory "posts/"

    let file = head $ filter (\h -> isPrefixOf (unpack slug) ("/"++h)) files
    
    content <- liftIO $ readFile ("posts"</>file)
    txt <- liftIO $ mdToHtml $ dropTagsLine content  
    html $ fromStrict $txt <>  (hashtagToAnchor $ findTags content)

tagsHandler :: ActionM ()
tagsHandler = text "demo"

collectPostsFrom :: FilePath -> IO [Post]
collectPostsFrom source = do
    content <- BL.readFile source
    let decoded = decode content 
    case decoded of 
      Just a -> return a
      Nothing -> return []

mdToHtml :: Text -> IO DT.Text
mdToHtml txt = runIOorExplode $ 
    readMarkdown def{readerExtensions=githubMarkdownExtensions} (toStrict txt) >>= writeHtml5String def

findTags :: String -> DT.Text
findTags content = DT.pack $ last $ lines content

dropTagsLine :: String -> Text
dropTagsLine content = pack $ unlines $ take ((length $ lines content)-1) $ lines content

hashtagToAnchor :: DT.Text -> DT.Text
hashtagToAnchor tags = DT.unlines $ fmap (\a -> "<a href='/tag:"<>(DT.strip a)<>"'>"<>(DT.strip a)<>"</a> " )$ DT.splitOn "#" tags
