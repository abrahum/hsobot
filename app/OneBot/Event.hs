{-# LANGUAGE OverloadedStrings #-}

module OneBot.Event where

import           Data.Aeson
import           Control.Lens.Lens
import           Data.Aeson.KeyMap (insert)
import qualified Data.Text as T

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

_insert :: (ToJSON a) => Key -> a -> Object -> Object
_insert k v = insert k (toJSON v)

instance ToJSON Event where
  toJSON (Event id impl platform self_id time ty detail_type sub_type c) =
    Object
    $ _insert "id" id
    $ _insert "impl" impl
    $ _insert "platform" platform
    $ _insert "self_id" self_id
    $ _insert "time" time
    $ _insert "type" ty
    $ _insert "detail_type" detail_type
    $ _insert "sub_type" sub_type c