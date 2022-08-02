{-# LANGUAGE OverloadedStrings #-}

module OneBot.Event where

import           Data.Aeson
import           Control.Lens.Lens
import qualified Data.Aeson.KeyMap as K
import qualified Data.Text as T
import           OneBot.Action (Resp)
import           Data.Maybe (isJust)

data Event = Event { event_id :: String
                   , impl :: String
                   , platform :: String
                   , self_id :: String
                   , time :: Float
                   , ty :: String
                   , detail_type :: String
                   , sub_type :: String
                   , content :: Object
                   }
  deriving (Show)

instance FromJSON Event where
  parseJSON = withObject "Event"
    $ \v -> Event <$> v .: "id"
    <*> v .: "impl"
    <*> v .: "platform"
    <*> v .: "self_id"
    <*> v .: "time"
    <*> v .: "type"
    <*> v .: "detail_type"
    <*> v .: "sub_type"
    ?? v

insert :: (ToJSON a) => Key -> a -> Object -> Object
insert k v = K.insert k (toJSON v)

instance ToJSON Event where
  toJSON (Event id impl platform self_id time ty detail_type sub_type c) =
    Object
    $ insert "id" id
    $ insert "impl" impl
    $ insert "platform" platform
    $ insert "self_id" self_id
    $ insert "time" time
    $ insert "type" ty
    $ insert "detail_type" detail_type
    $ insert "sub_type" sub_type c
