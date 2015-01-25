{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}
module Main where

import Control.Exception.Lifted
import Control.Exception.Base(SomeException)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.STM

import Control.Lens

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe
import Data.Foldable(toList)
import qualified Data.Sequence as Seq

import System.IO hiding (utf8)
import System.IO.Unsafe
import qualified System.ZMQ4 as ZMQ

import Network.BSD
import Network.Socket
import Network.IRC

import Network.RPC.Protopap.Server
import Network.RPC.Protopap.Publisher

import Text.ProtocolBuffers.Basic

import KlaczNG.Helpers
import KlaczNG.IRC.Proto.IrcMessage
import KlaczNG.IRC.Proto.IrcMessage as IM
import KlaczNG.IRC.Proto.UserCommand
import KlaczNG.IRC.Proto.UserCommand as UC
import KlaczNG.IRC.Proto.SendGatewayMessageRequest 
import KlaczNG.IRC.Proto.SendGatewayMessageResponse

zmqRpcEndpoint = "tcp://*:9001"
zmqPubEndpoint = "tcp://*:2137"

bsCrLf = BS.pack [13, 10]
hPutBSCrLf handle bs = BS.hPut handle bs >> BS.hPut handle bsCrLf

botNickname = "klacz"
botUser = "klacz"
botRealName = "Klacz"
botIrcHost = "localhost"
botChannels = ["#qwpx-dev"]

data GatewayEnv = GatewayEnv {
  _ircReaderChan :: TChan Message,
  _ircWriterChan :: TChan Message,
  _pubSocket :: ZMQ.Socket ZMQ.Pub
                }

$(makeLenses ''GatewayEnv)

childDiesTMVar = unsafePerformIO newEmptyTMVarIO

ircWriter handle chan = forever $ do
  msg <- atomically . readTChan $ chan
  putStrLn (show msg)
  hPutBSCrLf handle (encode msg)

ircReader handle chan = forever $ do
  line <- BS.hGetLine handle
  when (BS.null line) $
    error "Socket: Got empty line"
  let lineNoCr = BS.take (BS.length line - 1) line
  case decode lineNoCr of
    Just msg -> do
      putStrLn (show msg)
      atomically $ writeTChan chan msg
    Nothing -> error $ "couldn't parse irc line: " ++ show (BS.unpack line)

type IRCGateway = ReaderT GatewayEnv IO
runIRCGateway = runReaderT

sendMessage :: Message -> IRCGateway ()
sendMessage msg = do
  writerChan <- view ircWriterChan
  liftIO . atomically . writeTChan writerChan $ msg

readMessage :: IRCGateway Message
readMessage = do
  readerChan <- view ircReaderChan
  liftIO . atomically . readTChan $ readerChan

registerToIrc :: IRCGateway ()
registerToIrc = do
  sendMessage (nick botNickname)
  sendMessage (user botUser "8" "*" botRealName)

joinBotChannels :: Message -> IRCGateway ()
joinBotChannels _ = mapM_ (sendMessage . joinChan) botChannels

handlePing :: Message -> IRCGateway ()
handlePing pingMessage = case msg_params pingMessage of
  (server:_) -> sendMessage (pong server)
  _ -> error $ "incorrect ping message" ++ show pingMessage

gatewayActions = M.fromList [
  ("001", joinBotChannels),
  ("PING", handlePing)
  ]

maybeGatewayAction :: Message -> IRCGateway ()
maybeGatewayAction ircMessage = do
  let command = msg_command ircMessage
  case M.lookup command gatewayActions of
    Nothing -> return ()
    Just act -> act ircMessage

parseCommand :: Message -> Maybe UserCommand
parseCommand ircMessage = do
  prefix <- msg_prefix ircMessage
  caller <- case prefix of
    NickName nick _ _ -> Just nick
    _ -> Nothing
  (target, msg) <- case msg_params ircMessage of
    (a:b:_) -> Just (a, b)
    _       -> Nothing
  let msgText = textFromByteString msg
  when (T.null msgText) $ Nothing
  when (T.index msgText 0 /= ',') $ Nothing
  let (cmd, args) = T.breakOn (T.pack " ") (T.drop 1 msgText)
      args' = T.drop 1 args
  return $ UserCommand {
    UC.caller = Just $ uFromByteString caller,
    UC.replyTo = Just $ if target == botNickname
                        then uFromByteString caller
                        else uFromByteString target,
    UC.command = Just $ uFromText cmd,
    UC.args = Just $ uFromText args'
    }

maybePublishCommand :: Message -> IRCGateway ()
maybePublishCommand ircMessage = do
  case msg_command ircMessage of
    "PRIVMSG" -> maybe (return ()) publishUserCommand $ parseCommand ircMessage
    _ -> return ()
  where publishUserCommand command = do
          liftIO $ putStrLn ("Publishing " ++ show command)
          sock <- view pubSocket
          rpcPublish sock "UserCommand" command

publishMessage :: Message -> IRCGateway ()
publishMessage ircMessage = do
  let ircMessageProto = messageToIrcMessageProto ircMessage
  sock <- view pubSocket
  rpcPublish sock "IrcMessage" ircMessageProto
  return ()

messageToIrcMessageProto (Message { msg_prefix = prefix,
                                    msg_command = command,
                                    msg_params = params }) =
  IrcMessage { IM.command = Just . uFromByteString $ command,
               IM.prefix = uFromByteString . showPrefix <$> prefix,
               IM.params = Seq.fromList . map uFromByteString $ params }

parsePrefix :: BS.ByteString -> Either String Prefix
parsePrefix p = parseOnly Network.IRC.prefix p

ircMessageProtoToMessage :: IrcMessage -> Either String Message
ircMessageProtoToMessage (IrcMessage { IM.command = Just command,
                                       IM.prefix = maybePrefix,
                                       IM.params = params }) =
  let msg = Message { msg_command = LBS.toStrict $ utf8 command,
                      msg_prefix = Nothing,
                      msg_params = map (LBS.toStrict . utf8) (toList params) } in
  case maybePrefix of
    Nothing -> return msg
    Just p -> do
      prefix <- parsePrefix (LBS.toStrict . utf8 $ p)
      return $ msg { msg_prefix = Just prefix }

gateway :: IRCGateway ()
gateway = do
  registerToIrc
  gatewayLoop
  where gatewayLoop :: IRCGateway ()
        gatewayLoop = forever $ do
          ircMessage <- readMessage
          maybeGatewayAction ircMessage
          maybePublishCommand ircMessage
          publishMessage ircMessage

sendGatewayMessage :: TChan Message -> SendGatewayMessageRequest
                      -> IO (Either String SendGatewayMessageResponse)
sendGatewayMessage writerChan request = do
  case irc_message request of
    Nothing -> return . Left $ "no message in SendGatewayMessageRequest"
    Just msgProto -> case ircMessageProtoToMessage msgProto of
      Left err -> return (Left err)
      Right msg -> do
        atomically $ writeTChan writerChan msg
        return . Right $ SendGatewayMessageResponse Nothing

gatewayRPCServer writerChan sock = do
  let serviceDef = makeServiceDefinition [
        ("SendGatewayMessage", RPCHandler (sendGatewayMessage writerChan))
        ]
  forever $ do
    handleRPCCall serviceDef sock


start handle = do
  readerChan <- atomically $ newTChan
  forkChild "irc reader" $ ircReader handle readerChan

  writerChan <- atomically $ newTChan
  forkChild "irc writer" $ ircWriter handle writerChan

  bracket create destroy $ \(ctx, rpcSock, pubSock) -> do
    forkChild "rpc server" $ gatewayRPCServer writerChan rpcSock
    let env = GatewayEnv readerChan writerChan pubSock
    runIRCGateway gateway env
    return ()
  where create = do
          ctx <- ZMQ.context
          rpcSock <- ZMQ.socket ctx ZMQ.Rep
          ZMQ.bind rpcSock zmqRpcEndpoint
          pubSock <- ZMQ.socket ctx ZMQ.Pub
          ZMQ.bind pubSock zmqPubEndpoint
          return (ctx, rpcSock, pubSock)
        destroy (ctx, rpcSock, pubSock) = do
          ZMQ.close rpcSock
          ZMQ.close pubSock
          ZMQ.term ctx
        forkChild :: String -> IO a -> IO ThreadId
        forkChild name m = forkFinally m (notifyParent name)
        notifyParent :: String -> Either SomeException a -> IO ()
        notifyParent name s =
          let msg = case s of
                Left e -> show e
                Right _ -> "Success" in
          atomically $ putTMVar childDiesTMVar (name ++ ": " ++ msg)


main :: IO ()
main = withSocketsDo $ do
  addrs <- hostAddresses <$> getHostByName botIrcHost
  when (null addrs) $ error "unknown address"
  sock <- socket AF_INET Stream 0
  connect sock (SockAddrInet (toEnum 6667) (head addrs))
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering
  start handle
  s <- atomically $ takeTMVar childDiesTMVar
  putStrLn $ "Child died: " ++ s
  return ()
