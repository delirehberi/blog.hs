
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE QuantifiedConstraints#-}
{-#LANGUAGE UndecidableInstances#-}
{-#LANGUAGE FlexibleInstances#-}

module Main where

import qualified Web.Scotty as SC
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
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Internal
import qualified System.Environment as Env
import Text.RawString.QQ as QQ
import Data.Functor.Identity


data Post a = Post { uid :: a Text
                 , title :: Text
                 , created :: Text
                 , body :: Text
                 , slug :: Text
                 , draft :: a Bool
                 } deriving (Generic)

defPost :: Post Identity
defPost = Post {
                uid = pure "null",
                title = "",
                created = "",
                body = "",
                slug = "",
                draft = pure True
               }

instance (forall a. FromJSON a => FromJSON (f a)) => FromJSON (Post f) where
    parseJSON = genericParseJSON defaultOptions

instance (forall a. ToJSON a => ToJSON (f a)) => ToJSON (Post f) where
    toJSON = genericToJSON defaultOptions

instance (FromField a) => FromField (Identity a) where
    fromField f = Identity <$> fromField f

instance (ToField a) => ToField (Identity a) where
    toField (Identity a) = toField a

instance (forall a. FromRow a=>FromRow (Identity a)) => FromRow (Post Identity) where
    fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field

instance (forall a. ToRow a => ToRow (Identity a)) => ToRow (Post Identity) where
    toRow (Post id_ title created body slug draft) = toRow (id_,title,created,body,slug,draft)

instance (FromField a)=>FromRow (Identity a) where
    fromRow = field 
instance (ToField a) => ToRow (Identity a) where
    toRow (Identity a) = [toField a]


defaultPort :: Int
defaultPort = 3001

databaseName :: FilePath
databaseName = "./blog.db"

dbConnection :: IO Connection
dbConnection = open databaseName

main :: IO ()
main = do
    args <- Env.getArgs
    run args

run :: [String] -> IO ()
run [] = do 
    putStrLn "App started at 3001"
    conn <- dbConnection

--    result <- query_ conn "select * from content" :: IO [Post]
--    mapM_ print result
    
    configuredPort <- Env.lookupEnv "PORT"
    let port = case configuredPort of
                  Just x -> read x
                  Nothing -> defaultPort

    SC.scotty port $ app conn
    close conn

run ["help"] = putStr [QQ.r|
help                prints help
server              starts web server, will respect to PORT env, default port will be 3001
import "filename"   will import contents from filename
    |]

run ["import", filename] = importFromFile filename 

app :: Connection -> SC.ScottyM ()
app conn = do
    SC.get "/" $ indexHandler conn
    SC.get (SC.regex "^/([a-zA-Z0-9|-]{3,})$") blogHandler
    SC.get (SC.regex "^/tag:([a-zA-Z0-9|-]{3,})$") tagsHandler


indexHandler :: Connection -> SC.ActionM ()
indexHandler conn = do
    contents <- liftIO $ (query_ conn "SELECT * FROM content" :: (IO [Post Identity]))
    SC.text $ "Total content count is:"<> (pack (show $ length contents))

blogHandler :: SC.ActionM ()
blogHandler = do
    slug <- SC.param "0"
    files <- liftIO $ listDirectory "posts/"

    let file = head $ filter (\h -> isPrefixOf (unpack slug) ("/"++h)) files
    
    content <- liftIO $ readFile ("posts"</>file)
    txt <- liftIO $ mdToHtml $ dropTagsLine content  
    SC.html $ fromStrict $txt <>  (hashtagToAnchor $ findTags content)

tagsHandler :: SC.ActionM ()
tagsHandler = SC.text "demo"

collectPostsFrom :: FilePath -> IO [Post Maybe]
collectPostsFrom source = do
    content <- BL.readFile source
    let decoded = eitherDecode content 
    case decoded of 
      Right a -> return a
      Left x-> do 
          putStrLn x
          return []

mdToHtml :: Text -> IO DT.Text
mdToHtml txt = runIOorExplode $ 
    readMarkdown def{readerExtensions=githubMarkdownExtensions} (toStrict txt) >>= writeHtml5String def

findTags :: String -> DT.Text
findTags content = DT.pack $ last $ lines content

dropTagsLine :: String -> Text
dropTagsLine content = pack $ unlines $ take ((length $ lines content)-1) $ lines content

hashtagToAnchor :: DT.Text -> DT.Text
hashtagToAnchor tags = DT.unlines $ fmap (\a -> "<a href='/tag:"<>(DT.strip a)<>"'>"<>(DT.strip a)<>"</a> " )$ DT.splitOn "#" tags

importFromFile :: String -> IO ()
importFromFile filename = do
    conn <- dbConnection
    posts <- collectPostsFrom filename
    mapM_ (writeToDb conn) $ (fmap (\post -> fullBeSure defPost post) posts)
    where 
        writeToDb conn p = do 
            execute conn "INSERT INTO content (id,title,created,body,slug) VALUES(?,?,?,?,?)" p

beSure :: Identity a -> Maybe a -> Identity a
beSure (Identity a) maybeB = case maybeB of 
                               Just c -> Identity c
                               Nothing -> Identity a 

fullBeSure :: Post Identity -> Post Maybe -> Post Identity
fullBeSure orig over = Post {
                                uid = beSure (uid orig) (uid over),
                                title = title over,
                                created = created over,
                                body = body over,
                                slug = slug over,
                                draft = beSure (draft orig) (draft over)
                            } 
