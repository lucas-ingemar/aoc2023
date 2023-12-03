{-# LANGUAGE OverloadedStrings #-}

module InputData(getInputData) where


import Network.HTTP.Simple
import Network.HTTP.Client.Conduit (insertCookiesIntoRequest, createCookieJar, Cookie (Cookie, cookie_name, cookie_value, cookie_expiry_time, cookie_domain, cookie_path, cookie_creation_time, cookie_last_access_time, cookie_persistent, cookie_host_only, cookie_secure_only, cookie_http_only), Request (cookieJar), CookieJar)
import Data.ByteString.Lazy.Char8 (  ByteString, unpack  )
import Data.ByteString.Char8 (pack )
import Data.Time (getCurrentTime)
import System.Environment (getEnv)


getInputData :: Int -> IO String
getInputData day = do
    ruCookie <- getEnv "RU_COOKIE"
    sessionCookie <- getEnv "SESSION_COOKIE"
    now <- getCurrentTime
    let cookieJar  = createCookieJar [
          Cookie {
              cookie_name = "ru",
              cookie_value = pack ruCookie
              , cookie_expiry_time = now
              , cookie_domain = "adventofcode.com"
              , cookie_path = "/"
              , cookie_creation_time = now
              , cookie_last_access_time = now
              , cookie_persistent = False
              , cookie_host_only = False
              , cookie_secure_only = False
              , cookie_http_only = False
              },
          Cookie {
              cookie_name = "session",
              cookie_value = pack sessionCookie
              , cookie_expiry_time = now
              , cookie_domain = "adventofcode.com"
              , cookie_path = "/"
              , cookie_creation_time = now
              , cookie_last_access_time = now
              , cookie_persistent = False
              , cookie_host_only = False
              , cookie_secure_only = False
              , cookie_http_only = False
              }
          ]

    req <- parseRequest (getUrl day)
    let (request, jar2) = insertCookiesIntoRequest req cookieJar now

    response <- httpLbs request
    let rb = getResponseBody response
    return (unpack rb)

getUrl :: (Int) -> (String)
getUrl day = "https://adventofcode.com/2023/day/" ++ show day ++ "/input"
