{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Data.Time
import Data.Time.Clock.POSIX
import Text.Read (readMaybe)
import Network.Wai.Middleware.Servant.Errors (errorMw, HasErrorBody(..))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)

type API
  = "api" :> "timestamp" :> Get '[JSON] TimeStamp
  :<|> "api" :> "timestamp" :> Capture "dateString" String :> Get '[JSON] TimeStamp

data TimeStamp
  = TimeStamp
    { unix :: String
    , utc :: String
    } deriving (Generic, ToJSON)

startApp :: IO ()
startApp = putStrLn "Server running on Port 8080" >>
  run 8080 (errorMw @JSON @["error", "status"]
  $ app)

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = sendCurrentDate :<|> parseDate
  where
    parseDate :: String -> Handler TimeStamp
    parseDate s =
      case catMaybes [parseInputString s, parseInputUnixTime s] of 
        [utcTime] -> 
          return (TimeStamp (getPosixTime utcTime) (show utcTime))
        _ -> 
          throwError $ err400 { errBody = "Unknown Date" }
    sendCurrentDate :: Handler TimeStamp
    sendCurrentDate = do
      currentTime <- liftIO getCurrentTime
      return $ TimeStamp (getPosixTime currentTime) (show currentTime)

getPosixTime :: UTCTime -> String
getPosixTime = show . round . utcTimeToPOSIXSeconds

parseInputString :: String -> Maybe UTCTime
parseInputString = parseTimeM True defaultTimeLocale "%Y-%m-%d"

parseInputUnixTime :: String -> Maybe UTCTime
parseInputUnixTime = parseTimeM True defaultTimeLocale "%s"

functions = [ parseInputString, parseInputUnixTime ]