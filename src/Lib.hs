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
import Control.Applicative ((<|>))


-- App Start up

startApp :: IO ()
startApp = putStrLn "Server running on Port 8080" >>
  run 8080 (errorMw @JSON @["error", "status"]
  $ app)

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- Data Types


data TimeStamp
  = TimeStamp
    { unix :: String
    , utc :: String
    } deriving (Generic, ToJSON)


-- API

type API
  = "api" :> "timestamp" :>
    (                                     Get '[JSON] TimeStamp      
      :<|> Capture "dateString" String :> Get '[JSON] TimeStamp 
    )


-- Request Handlers --

server :: Server API
server = sendCurrentDate :<|> parseDate
  where
    parseDate :: String -> Handler TimeStamp
    parseDate s =
      maybe throw (return . makeTimestamp)
        (parseInputString s <|> parseInputUnixTime s)
        where
          makeTimestamp utcTime = TimeStamp (getPosixTime utcTime) (show utcTime)
          throw = throwError  $ err400 { errBody = "Unknown Date" }
    sendCurrentDate :: Handler TimeStamp
    sendCurrentDate = do
      currentTime <- liftIO getCurrentTime
      return $ TimeStamp (getPosixTime currentTime) (show currentTime)


-- Helpers

getPosixTime :: UTCTime -> String
getPosixTime = show . round . utcTimeToPOSIXSeconds

parseInputString :: String -> Maybe UTCTime
parseInputString = parseTimeM True defaultTimeLocale "%Y-%m-%d"

parseInputUnixTime :: String -> Maybe UTCTime
parseInputUnixTime = parseTimeM True defaultTimeLocale "%s"