{-# LANGUAGE OverloadedStrings #-}

module OneBot.Action where

import           Data.Aeson

type ActionType = String

type ActionParams = Object

data Action = Action ActionType ActionParams
  deriving (Show)

instance ToJSON Action where
  toJSON (Action t p) = object ["action" .= t, "params" .= p]

instance FromJSON Action where
  parseJSON =
    withObject "action" $ \v -> Action <$> v .: "action" <*> v .: "params"

type Resp = Value