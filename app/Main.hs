module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified OneBot.MsgSeg as M
import qualified OneBot.Event as E
import           OneBot.OBC (runAppOBC)

main :: IO ()
main = do
  messagejson <- B.readFile "tests/message.json"
  let message = decode messagejson :: Maybe M.MsgSegs
  print message
  print $ encode $ M.text "Hello Haskell"
  eventjson <- B.readFile "tests/event.json"
  let event = decode eventjson :: Maybe E.Event
  print event
  runAppOBC