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
      msg <- WS.receive conn
      case msg of
        WS.DataMessage _ _ _ (WS.Text s _) -> do
          let event = decode s :: Maybe Event
          print event
        WS.DataMessage {} -> do
          print "get!"
          WS.sendTextData conn ("get!" :: T.Text)
        WS.ControlMessage (WS.Ping b) -> WS.sendBinaryData conn b
        WS.ControlMessage (WS.Pong _) -> print "pong!"
        WS.ControlMessage WS.Close {} -> return ()

runAppOBC :: IO ()
runAppOBC = WS.runServer "127.0.0.1" 8080 appOBC
