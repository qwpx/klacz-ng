{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.AMQP
import Data.UUID.V4
import Data.Text as T
import Data.Functor
import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad as M
import Data.Text.Encoding
import Data.List
import System.Environment
import Data.Aeson
import Crypto.Hash.SHA1 as CH
import Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL

data QMsg = QMsg {
  from :: Text,
  replyTo :: Text,
  content :: Text
  } deriving Show

instance FromJSON QMsg where
  parseJSON (Object v) = QMsg <$>
                         v .: "from" <*>
                         v .: "replyTo" <*>
                         v .: "content"
  parseJSON _ = M.mzero

instance ToJSON QMsg where
  toJSON (QMsg from replyTo content) =
    object ["from" .= from, "replyTo" .= replyTo, "content" .= content]

main :: IO ()
main =
  getArgs >>= parse >>= startModule

parse [] = openConnection'' defaultConnectionOpts
parse (uri:_) = openConnection'' $ fromURI $ uri

startModule :: Connection -> IO ()
startModule conn = do
  done <- newEmptyMVar
  ch <- openChannel conn
  uuid <- nextRandom
  declareExchange ch newExchange {exchangeName = "events", exchangeType = "topic", exchangeDurable = False}
  declareQueue ch newQueue {queueName = T.pack $ show uuid,
                            queueAutoDelete = True,
                            queueDurable = False}
  consumeMsgs ch (T.pack $ show uuid) NoAck (deliveryHandler conn)
  bindQueue ch (T.pack $ show uuid) "events" "klacz.privmsg.pick"
  takeMVar done
  closeConnection conn

deliveryHandler :: Connection -> (Message, Envelope) -> IO ()
deliveryHandler conn (msg, metadata) = do
  chan <- openChannel conn
  let json = decode (msgBody msg) in
   case json :: Maybe QMsg of
   Just m -> void $
             publishMsg chan "" "klacz.gateway" newMsg {msgBody = process m }
   Nothing -> return ()

process :: QMsg -> BL.ByteString
process msg = encode QMsg {
  from = from msg,
  replyTo = replyTo msg,
  content = pick $ content msg
  }

pick :: Text -> Text
pick = T.intercalate " < " . fmap fst . sortBy f . hashList . T.words where
  f (_, a) (_, b) = a `compare` b

hashList :: [Text] -> [(Text, B.ByteString)]
hashList = fmap (\ x -> (x, CH.hash $ encodeUtf8 x))
