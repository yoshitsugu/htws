{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad          ((>=>))
import qualified Data.Aeson             as JSON
import qualified Data.Aeson.Types       as T
import           Data.ByteString.Char8  (pack)
import           Data.Conduit
import           Data.Map.Lazy          (findWithDefault)
import           Data.String            (IsString)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           MyOAuthConfig
import           Network                (withSocketsDo)
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           System.Environment     (getArgs)
import           System.IO.Unsafe       (unsafePerformIO)
import           Web.Authenticate.OAuth as OAuth


oauth :: OAuth.OAuth
oauth = OAuth.newOAuth
    { OAuth.oauthServerName = "twitter"
    , OAuth.oauthRequestUri = "https://twitter.com/oauth/request_token"
    , OAuth.oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
    , OAuth.oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
    , OAuth.oauthSignatureMethod = OAuth.HMACSHA1
    , OAuth.oauthConsumerKey = ckey oauthConfig
    , OAuth.oauthConsumerSecret = csecret oauthConfig
    , OAuth.oauthVersion         = OAuth.OAuth10a
    }


endpoint = "https://api.twitter.com/1.1/search/tweets.json"

fetch :: Method -> Credential ->  SimpleQuery -> IO JSON.Value
fetch mth cred q = do
    man <- newManager tlsManagerSettings
    req <- parseUrl endpoint
    req' <- signOAuth oauth cred (req {method=mth,queryString = renderSimpleQuery True q})
    res <- httpLbs req' man
    maybe (fail "JSON decoding error") return $ JSON.decode (responseBody res)



fetchGET = fetch methodGet
fetchPOST = fetch methodPost

statuses :: JSON.Value -> T.Parser [TwStatus]
statuses v = do
  parsed <- JSON.parseJSON v
  s <- (JSON..:) parsed "statuses"
  mapM status s

data TwStatus = TwStatus { statusId :: Integer, screenName :: String, textBody :: String, createdAt :: LocalTime } deriving (Show)

status :: JSON.Value -> T.Parser TwStatus
status v = do
  parsed <- JSON.parseJSON v
  i <- (JSON..:) parsed "id"
  t <- (JSON..:) parsed "text"
  ca <- (JSON..:) parsed "created_at"
  u <- (JSON..:) parsed "user"
  n <- (JSON..:) u "screen_name"
  let tz = hoursToTimeZone 9
  let c = utcToLocalTime tz $ parseTimeOrError True defaultTimeLocale "%a %b %d %X +0000 %Y" ca
  return $ TwStatus i n t c


main = withSocketsDo $ do
    args <- getArgs
    if length args /= 1
      then putStrLn "Usage: htws keyword"
      else do
        let keyword = head args
        let cred = newCredential (atoken oauthConfig) (asecret oauthConfig)
        result <- fetchGET cred [("q", pack keyword)]
        case T.parse statuses result of
          T.Success _p -> printTws _p
          T.Error e -> print e
  where
    printTws :: [TwStatus] -> IO ()
    printTws = mapM_ (\s -> do
                          putStrLn "---------------"
                          putStrLn $ "https://twitter.com/" ++ screenName s ++ "/status/" ++ (show . statusId) s
                          putStrLn $ textBody s
                          print $ createdAt s)


