{-# LANGUAGE  TemplateHaskell #-}
module Config(IRCConfig(..)) where

import Data.Aeson.TH
import Data.Text

data IRCConfig = IRCConfig {
  ircServer :: Text,
  ircNickname :: Text,
  ircBotChannels :: [Text],
  ircNickServPass :: Text
  } deriving (Show)

$(deriveJSON defaultOptions ''IRCConfig)
