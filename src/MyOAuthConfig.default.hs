-- {-# LANGUAGE OverloadedStrings #-}
-- module MyOAuthConfig where
--
-- import qualified Data.ByteString as BS (ByteString)
--
-- data  MyOAuthConfig = MyOAuthConfig
--                    {
--                      ckey    :: BS.ByteString
--                    , csecret :: BS.ByteString
--                    , atoken  :: BS.ByteString
--                    , asecret :: BS.ByteString
--                    } deriving (Show)
--
-- oauthConfig :: MyOAuthConfig
-- oauthConfig = MyOAuthConfig (error "ckey") (error "csecret") (error "atoken") (error "asecret")
