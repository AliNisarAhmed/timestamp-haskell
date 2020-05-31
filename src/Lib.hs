{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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

type API 
  = "api" :> "timestamp" :> Capture "dateString" String :> Get '[JSON] TimeStamp

data TimeStamp = 
  TimeStamp { unix :: String 
    , utc :: String 
    } 
  | ParseError {
    error :: String
  } deriving Generic

instance ToJSON TimeStamp

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = parseDate
  where 
    parseDate :: String -> Handler TimeStamp
    parseDate s =
      case parseInputString s of 
        Just utcTime -> 
          return (TimeStamp (getPosixTime utcTime) (show utcTime))
        Nothing -> 
          case parseInputUnixTime s of 
            Just utcTime -> 
              return (TimeStamp (getPosixTime utcTime) (show utcTime))
            Nothing -> 
              throwError $ err400 { errBody = "Unknown Date" }


getPosixTime :: UTCTime -> String
getPosixTime = show . round . utcTimeToPOSIXSeconds

parseInputString :: String -> Maybe UTCTime
parseInputString = parseTimeM True defaultTimeLocale "%Y-%m-%d"

parseInputUnixTime :: String -> Maybe UTCTime
parseInputUnixTime = parseTimeM True defaultTimeLocale "%s"