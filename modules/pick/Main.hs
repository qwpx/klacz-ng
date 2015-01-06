{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad as M
import Crypto.Hash.SHA1 as CH

import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Sequence (fromList)
import Data.Ord
import Data.Text as T
import Data.Text.Encoding
import Data.UUID.V4

import Network.AMQP

import System.Environment

import Text.ProtocolBuffers.Basic (Utf8, utf8, toUtf8, defaultValue, uFromString)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)

import IRC.UserCommand
import IRC.UserCommand as UC
import IRC.GatewayCommand
import IRC.GatewayCommand as GC

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

makeReplyTo :: Text -> Text -> GatewayCommand
makeReplyTo replyTo reply = GatewayCommand {
  GC.command = Just $ uFromString "PRIVMSG",
  GC.params = fromList [textToUtf8 replyTo],
  GC.rest = Just $ textToUtf8 reply
  }

process :: UserCommand -> GatewayCommand
process (UserCommand { UC.caller = Just caller,
                       UC.command = Just command,
                       UC.replyTo = Just replyTo,
                       UC.args = Just args }) =
  let (caller', command', replyTo', args') =
        (utf8ToText caller, utf8ToText command, utf8ToText replyTo, utf8ToText args) in
  case lookup command' functions of
    Nothing -> makeReplyTo replyTo' "unknown command"
    Just f -> f caller' replyTo' args'
    where functions = [("pick", pickCommand), ("sage", sageCommand)]

pickCommand :: Text -> Text -> Text -> GatewayCommand
pickCommand _ replyTo args = makeReplyTo replyTo (pick args)

sageCommand :: Text -> Text -> Text -> GatewayCommand
sageCommand _ replyTo args = GatewayCommand {
  GC.command = Just $ uFromString "KICK",
  GC.params = fromList [textToUtf8 replyTo, textToUtf8 nick],
  GC.rest = Just $ textToUtf8 (T.drop 1 reason) }
  where (nick, reason) = T.breakOn " " args

utf8ToText :: Utf8 -> Text
utf8ToText u = decodeUtf8 (BL.toStrict (utf8 u))

textToUtf8 :: Text -> Utf8
textToUtf8 t = let (Right u) = toUtf8 (BL.fromStrict (encodeUtf8 t)) in u

pick :: Text -> Text
pick = T.intercalate " < " . sortBy (comparing (CH.hash . encodeUtf8)) . T.words where
