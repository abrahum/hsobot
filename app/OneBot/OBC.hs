{-# LANGUAGE OverloadedStrings #-}

module OneBot.OBC where

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import           Data.Aeson (decode)
import           OneBot.Event (Event)

appOBC :: WS.ServerApp
appOBC pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ())
    $ do
      msg <- WS.receiveDataMessage conn
      case msg of
        WS.Text s _ -> do
          let event = decode s :: Maybe Event
          print event
        WS.Binary b -> do
          print "get!"
          WS.sendTextData conn ("get!" :: T.Text)

runAppOBC :: IO ()
runAppOBC = WS.runServer "127.0.0.1" 8080 appOBC
