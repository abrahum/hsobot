{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module OneBot.MsgSeg where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as K
import qualified Data.Text as T

type MsgSegs = [MsgSeg]

type SegType = String

type SegData = Object

data MsgSeg = MsgSeg SegType SegData
  deriving (Show)

instance FromJSON MsgSeg where
  parseJSON =
    withObject "message" $ \obj -> MsgSeg <$> obj .: "type" <*> obj .: "data"

instance ToJSON MsgSeg where
  toJSON (MsgSeg t d) = object ["type" .= t, "data" .= d]

class IntoMsgSegs a where
  msgSeg :: a -> MsgSegs
  (+) :: IntoMsgSegs b => a -> b -> MsgSegs
  (+) a b = msgSeg a ++ msgSeg b

instance IntoMsgSegs MsgSegs where
  msgSeg segs = segs

instance IntoMsgSegs MsgSeg where
  msgSeg seg = [seg]

instance IntoMsgSegs String where
  msgSeg s = [text s]

insert :: ToJSON v => MsgSeg -> Key -> v -> MsgSeg
insert (MsgSeg t d) k v = MsgSeg t $ K.insert k (toJSON v) d

text :: String -> MsgSeg
text t = MsgSeg "text" $ K.fromList ["text" .= t]

mention :: String -> MsgSeg
mention user_id = MsgSeg "mention" $ K.fromList ["user_id" .= user_id]

mentionAll = MsgSeg "mention_all" $ K.fromList []

image :: String -> MsgSeg
image i = MsgSeg "image" $ K.fromList ["file_id" .= i]

voice :: String -> MsgSeg
voice i = MsgSeg "voice" $ K.fromList ["file_id" .= i]

audio :: String -> MsgSeg
audio i = MsgSeg "audio" $ K.fromList ["file_id" .= i]

video :: String -> MsgSeg
video i = MsgSeg "video" $ K.fromList ["file_id" .= i]
