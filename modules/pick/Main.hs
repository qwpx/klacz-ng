{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad as M
import Crypto.Hash.SHA1 as CH

import Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Sequence (fromList)
import Data.Text as T
import Data.Text.Encoding
import Data.UUID.V4

import Network.AMQP

import System.Environment

import Text.ProtocolBuffers.Basic (Utf8, utf8, toUtf8, defaultValue, uFromString)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)

import IRC.UserCommand
import qualified IRC.UserCommand as UC
import IRC.GatewayCommand
import qualified IRC.GatewayCommand as GC

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
  bindQueue ch (T.pack $ show uuid) "events" "klacz.command"
  takeMVar done
  closeConnection conn

deliveryHandler :: Connection -> (Message, Envelope) -> IO ()
deliveryHandler conn (msg, metadata) = do
  chan <- openChannel conn
  case messageGet (msgBody msg) of
    Right (userCommand, x) | BL.length x == 0 ->
      void $ publishMsg chan "" "klacz.gateway" newMsg {msgBody = messagePut . process $ userCommand }
    _ -> return ()

makeReplyTo :: Utf8 -> Utf8 -> GatewayCommand
makeReplyTo replyTo reply = GatewayCommand {
  GC.command = Just $ uFromString "PRIVMSG",
  GC.params = fromList [replyTo],
  GC.rest = Just $ reply
  }

process :: UserCommand -> GatewayCommand
process (UserCommand { UC.command = Just command,
                       UC.replyTo = replyTo,
                       UC.args = args}) =
  let replyTo' = fromJust $ replyTo in
  case utf8 command of
    "pick" -> makeReplyTo replyTo' (textToUtf8 . pick . utf8ToText $ fromMaybe (uFromString "") args)
    _ -> makeReplyTo replyTo' (uFromString "unknown command")

utf8ToText :: Utf8 -> Text
utf8ToText u = decodeUtf8 (BL.toStrict (utf8 u))

textToUtf8 :: Text -> Utf8
textToUtf8 t = let (Right u) = toUtf8 (BL.fromStrict (encodeUtf8 t)) in u

pick :: Text -> Text
pick = T.intercalate " < " . fmap fst . sortBy f . hashList . T.words where
  f (_, a) (_, b) = a `compare` b

hashList :: [Text] -> [(Text, B.ByteString)]
hashList = fmap (\ x -> (x, CH.hash $ encodeUtf8 x))
