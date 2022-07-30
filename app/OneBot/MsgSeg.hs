{-# LANGUAGE OverloadedStrings #-}

module OneBot.MsgSeg where

import           Data.Aeson
import           Data.Aeson.KeyMap (fromList)
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

textSegment :: String -> MsgSeg
textSegment t = MsgSeg "text" $ fromList [("text", toJSON t)]

mentionAllSegment = MsgSeg "mention_all" $ fromList []

