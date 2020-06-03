{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, reader, runReaderT,lift,ask)
import qualified Data.ByteString.Char8 as BC
import Data.Functor.Identity
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen =
  replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI  :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

data DbConfig = DbConfig { conn :: R.Connection }

{-
testConnection :: BC.ByteString -> ReaderT DbConfig IO (Either R.Reply (Maybe BC.ByteString))
testConnection str = do
  rConn <- reader conn
  liftIO (getURI rConn str)
-}

{-
get    :: RoutePattern -> ActionM () -> ScottyM ()

reader :: (r -> a) -> ReaderT r m a
param  :: Text -> ActionM a
html   :: Text -> ActionM ()
text   :: Text -> ActionM ()

ActionT === newtype (monad)
ActionM === ActionT Text IO
ScottyT === newtype (monad)
ScottyM === ScottyT Text IO
-}

handleCreate :: R.Connection -> ActionM ()
handleCreate rConn = do
  uri <- param "uri"
  let parsedUri :: Maybe URI
      parsedUri = parseURI (TL.unpack uri)
  case parsedUri of
    Just _  -> do
      shawty <- liftIO shortyGen
      let shorty = BC.pack shawty
          uri' = encodeUtf8 (TL.toStrict uri)
      resp <- liftIO (saveURI rConn shorty uri')
      html (shortyCreated resp shawty)
    Nothing -> text (shortyAintUri uri)

handleRetrieve :: R.Connection -> ActionM ()
handleRetrieve rConn = do
  short <- param "short"
  uri <- liftIO (getURI rConn short)
  case uri of
    Left reply -> text (TL.pack (show reply))
    Right mbBS -> case mbBS of
      Nothing -> text "uri not found"
      Just bs -> html (shortyFound tbs)
        where tbs :: TL.Text
              tbs = TL.fromStrict (decodeUtf8 bs)

{-
-- This compiles, but why do I need to manually lift?
-- If I add in the uri query using the param func, it blows up.
myAction :: ReaderT DbConfig ActionM ()
myAction = do
  rConn <- reader conn
  -- uri <- param "uri"
  -- let parsedUri :: Maybe URI
  --     parsedUri = parseURI (TL.unpack uri)
  lift . text $ "oh crap"

-- This function works but it only layers DbConfig Reader on top of IO
go :: ReaderT DbConfig IO ()
go = do
  rConn <- reader conn
  res <- liftIO (getURI rConn "Tom")
  case res of
    Left reply -> liftIO (putStrLn "reply")
    Right mbs -> case mbs of
      Nothing -> liftIO (putStrLn "no result")
      Just bs -> liftIO (putStrLn $ show bs)


-- It sucks that I have to use runReaderT again here, as well as in main?
-- Can't I just thread the same reader through?
myTestApp :: ReaderT DbConfig ScottyM ()
myTestApp = do
  -- a <- ask
  -- lift $ get "/" (runReaderT myAction a)
-}

app :: ReaderT DbConfig ScottyM ()
app = do
  rConn <- reader conn
  lift $ get "/" $ handleCreate rConn
  lift $ get "/:short" $ handleRetrieve rConn

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  let webapp = runReaderT app $ DbConfig rConn
  scotty 3000 webapp
  -- runReaderT go dbconf
