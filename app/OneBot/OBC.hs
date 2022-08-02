{-# LANGUAGE OverloadedStrings #-}

module OneBot.OBC where

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import           Data.Aeson (eitherDecode, decode)
import           Data.List (findIndex)
import           Data.ByteString.Lazy (ByteString)
import           OneBot.Event (Event(self_id))
import           OneBot.Action (Resp)
import           OneBot.Types (Echo(Echo))
import           Control.Concurrent (MVar, newMVar, takeMVar, putMVar, readMVar)

type EventHandler = Event -> Bots -> MVar Echos -> IO ()

type Bot = (String, WS.Connection)

type Bots = [Bot]

botExists :: String -> Bots -> Bool
botExists self_id = any ((== self_id) . fst)

addBot :: String -> WS.Connection -> Bots -> Bots
addBot self_id bot bots = if botExists self_id bots
                          then bots
                          else (self_id, bot):bots

removeBot :: String -> Bots -> Bots
removeBot self_id = filter ((/= self_id) . fst)

type RespHandler = Resp -> IO ()

type EchoResp = (String, RespHandler)

type Echos = [EchoResp]

removeRespHandler :: String -> Echos -> (Maybe RespHandler, Echos)
removeRespHandler echo echos = inner index
  where
    index = findIndex (\e -> fst e == echo) echos

    inner Nothing = (Nothing, echos)
    inner (Just i) =
      (Just $ snd $ echos !! i, take (i - 1) echos ++ drop i echos)

addRespHandler :: String -> RespHandler -> Echos -> Echos
addRespHandler echo handler echos = (echo, handler):echos

appOBC :: EventHandler -> MVar Bots -> MVar Echos -> WS.ServerApp
appOBC handler bots echos pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ())
    $ do
      msg <- WS.receiveDataMessage conn
      case msg of
        WS.Text s _ -> do
          let event = decode s :: Maybe Event
          handleEvent handler bots echos conn $ maybe2Either event s
        WS.Binary b -> do
          print "binary received"
          WS.sendTextData conn ("get!" :: T.Text)
  where
    maybe2Either (Just a) b = Left a
    maybe2Either Nothing b = Right b

    handleEvent :: EventHandler
                -> MVar Bots
                -> MVar Echos
                -> WS.Connection
                -> Either Event ByteString
                -> IO ()
    handleEvent handler bots echos conn (Left event) = do
      new_bots <- takeMVar bots
      let new_bots = addBot (self_id event) conn new_bots
      putMVar bots new_bots
      handler event new_bots echos
    handleEvent _ _ echos _ (Right s) = handleResp echos $ decode s

    handleResp :: MVar Echos -> Maybe (Echo Resp) -> IO ()
    handleResp echos (Just (Echo s resp)) = do
      new_echos <- takeMVar echos
      let (h, new_echos) = removeRespHandler s new_echos
      putMVar echos new_echos
      handleResp' h resp
    handleResp _ _ = putStrLn "Event or Resp Deserialize failed"

    handleResp' :: Maybe RespHandler -> Resp -> IO ()
    handleResp' (Just h) resp = h resp
    handleResp' _ r = putStrLn "No EchoHandler registered for Resp"

runAppOBC :: EventHandler -> IO ()
runAppOBC handler = do
  putStrLn "starting WS server ws://127.0.0.1:8080/"
  bots <- newMVar []
  echos <- newMVar []
  WS.runServer "127.0.0.1" 8080 $ appOBC handler bots echos
