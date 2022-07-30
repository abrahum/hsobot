{-# LANGUAGE OverloadedStrings #-}

module OneBot.Types where

import           Data.Aeson
import           Data.Aeson.KeyMap (insert)

data Echo a = Echo String a

instance FromJSON a => FromJSON (Echo a) where
  parseJSON = withObject ""
    $ \v -> Echo <$> v .: "echo" <*> parseJSON (Object v)

instance ToJSON a => ToJSON (Echo a) where
  toJSON (Echo echo a) = _echo2json (toJSON a) echo

_echo2json :: Value -> String -> Value
_echo2json (Object o) echo = Object $ insert "echo" (toJSON echo) o
_echo2json v echo = object ["echo" .= echo, "data" .= v]