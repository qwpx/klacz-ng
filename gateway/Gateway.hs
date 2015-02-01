{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}
module Main where

import Control.Exception.Lifted
import Control.Exception.Base(SomeException)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.STM

import Control.Lens

import qualified Data.Aeson as Aeson
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

import Options.Applicative

import Text.ProtocolBuffers.Basic

import qualified Ephemeral.Client as Ephemeral

import KlaczNG.Helpers
import KlaczNG.IRC.Proto.IrcMessage
import KlaczNG.IRC.Proto.IrcMessage as IM
import KlaczNG.IRC.Proto.UserCommand
import KlaczNG.IRC.Proto.UserCommand as UC
import KlaczNG.IRC.Proto.SendGatewayMessageRequest 
import KlaczNG.IRC.Proto.SendGatewayMessageResponse

bsCrLf = BS.pack [13, 10]
hPutBSCrLf handle bs = BS.hPut handle bs >> BS.hPut handle bsCrLf

data GatewayEnv = GatewayEnv {
  _ircReaderChan :: TChan Message,
  _ircWriterChan :: TChan Message,
  _pubSocket :: ZMQ.Socket ZMQ.Pub,
  _ephemeralSocket :: ZMQ.Socket ZMQ.Req
  }

$(makeLenses ''GatewayEnv)

childDiesTMVar = unsafePerformIO newEmptyTMVarIO

ircWriter handle chan = forever $ do
  liftIO $ putStrLn $ "reading"
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

type IRCGateway = StateT GatewayEnv IO
runIRCGateway = runStateT

lookupEphemeral :: Aeson.FromJSON a => T.Text -> IRCGateway (Maybe a)
lookupEphemeral key =  do
  sock <- use ephemeralSocket
  val <- Ephemeral.getValue sock key
  return . join $ Aeson.decode <$> val

getEphemeral :: Aeson.FromJSON a => T.Text -> IRCGateway a
getEphemeral key = fromMaybe (error $ "expecting " <> T.unpack key <> " in ephemeral")
                   <$> lookupEphemeral key

setEphemeral :: Aeson.ToJSON a => T.Text -> a -> IRCGateway ()
setEphemeral key value = do
  sock <- use ephemeralSocket
  Ephemeral.setValue sock key $ Aeson.encode [value]

sendMessage :: Message -> IRCGateway ()
sendMessage msg = do
  msg `seq` return ()
  writerChan <- use ircWriterChan
  liftIO . atomically . writeTChan writerChan $ msg

readMessage :: IRCGateway Message
readMessage = do
  readerChan <- use ircReaderChan
  liftIO . atomically . readTChan $ readerChan

registerToIrc :: IRCGateway ()
registerToIrc = do
  nickname <- textToBS <$> getEphemeral "nickname"
  sendMessage (nick nickname)
  sendMessage (user nickname "8" "*" nickname)

joinBotChannels :: Message -> IRCGateway ()
joinBotChannels _ = do
  channels <- getEphemeral "channels"
  mapM_ (sendMessage . joinChan . textToBS) channels

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

parseCommand :: T.Text -> Message -> Maybe UserCommand
parseCommand nickname ircMessage = do
  prefix <- msg_prefix ircMessage
  caller <- case prefix of
    NickName nick _ _ -> Just nick
    _ -> Nothing
  (target, msg) <- case msg_params ircMessage of
    (a:b:_) -> Just (a, b)
    _       -> Nothing
  let msgText = textFromBS msg
  when (T.null msgText) $ Nothing
  when (T.index msgText 0 /= ',') $ Nothing
  let (cmd, args) = T.breakOn (T.pack " ") (T.drop 1 msgText)
      args' = T.drop 1 args
  return $ UserCommand {
    UC.caller = Just $ uFromByteString caller,
    UC.replyTo = Just $ if target == textToBS nickname
                        then uFromByteString caller
                        else uFromByteString target,
    UC.command = Just $ uFromText cmd,
    UC.args = Just $ uFromText args'
    }

maybePublishCommand :: Message -> IRCGateway ()
maybePublishCommand ircMessage = do
  nickname <- getEphemeral "nickname"
  case msg_command ircMessage of
    "PRIVMSG" -> maybe (return ()) publishUserCommand $
                 parseCommand nickname ircMessage
    _ -> return ()
  where publishUserCommand command = do
          liftIO $ putStrLn ("Publishing " ++ show command)
          sock <- use pubSocket
          rpcPublish sock "UserCommand" command

publishMessage :: Message -> IRCGateway ()
publishMessage ircMessage = do
  let ircMessageProto = messageToIrcMessageProto ircMessage
  sock <- use pubSocket
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
        atomically $ writeTChan writerChan $! msg
        return . Right $ SendGatewayMessageResponse Nothing

gatewayRPCServer writerChan sock = do
  let serviceDef = makeServiceDefinition [
        ("SendGatewayMessage", RPCHandler (sendGatewayMessage writerChan))
        ]
  forever $ do
    handleRPCCall serviceDef sock


start flags handle = do
  readerChan <- atomically $ newTChan
  forkChild "irc reader" $ ircReader handle readerChan

  writerChan <- atomically $ newTChan
  forkChild "irc writer" $ ircWriter handle writerChan
  atomically $ writeTChan writerChan (nick "kek")
  atomically $ writeTChan writerChan (error "lel")
  atomically $ writeTChan writerChan (nick "wut")
  let gatewayRpcE = flagGatewayRpcEndpoint flags
      gatewayPubE = flagGatewayPubEndpoint flags
      ephemeralRpcE = flagEphemeralRpcEndpoint flags
      nickname = T.pack $ flagNickname flags
      channels = T.split (==',') . T.pack $ flagBotChannels flags
  bracket (create gatewayRpcE gatewayPubE ephemeralRpcE) destroy $
    \(ctx, gatewayRpcSock, gatewayPubSock, ephemeralRpcSock) -> do
      Ephemeral.clear ephemeralRpcSock
      Ephemeral.setValue ephemeralRpcSock "nickname" $
        Aeson.encode [nickname]
      Ephemeral.setValue ephemeralRpcSock "channels" $
        Aeson.encode channels
      forkChild "rpc server" $ gatewayRPCServer writerChan gatewayRpcSock
      let env = GatewayEnv readerChan writerChan gatewayPubSock ephemeralRpcSock
      runIRCGateway gateway env
      return ()
  where create gatewayRpcE gatewayPubE ephemeralRpcE = do
          ctx <- ZMQ.context
          gatewayRpcSock <- ZMQ.socket ctx ZMQ.Rep
          ZMQ.bind gatewayRpcSock gatewayRpcE
          gatewayPubSock <- ZMQ.socket ctx ZMQ.Pub
          ZMQ.bind gatewayPubSock gatewayPubE
          ephemeralRpcSock <- ZMQ.socket ctx ZMQ.Req
          ZMQ.connect ephemeralRpcSock ephemeralRpcE
          return (ctx, gatewayRpcSock, gatewayPubSock, ephemeralRpcSock)
        destroy (ctx, gatewayRpcSock, gatewayPubSock, ephemeralRpcSock) = do
          ZMQ.close gatewayRpcSock
          ZMQ.close gatewayPubSock
          ZMQ.close ephemeralRpcSock
          ZMQ.term ctx
        forkChild :: String -> IO a -> IO ThreadId
        forkChild name m = forkFinally m (notifyParent name)
        notifyParent :: String -> Either SomeException a -> IO ()
        notifyParent name s =
          let msg = case s of
                Left e -> show e
                Right _ -> "Success" in
          atomically $ putTMVar childDiesTMVar (name ++ ": " ++ msg)

data Flags = Flags {
  flagGatewayRpcEndpoint :: String,
  flagGatewayPubEndpoint :: String,
  flagEphemeralRpcEndpoint :: String,
  flagIrcServer :: String,
  flagNickname :: String,
  flagBotChannels :: String
  }

flags = Flags
        <$> strOption (long "gateway-rpc-endpoint"
                       <> metavar "RPC_ENDPOINT"
                       <> help "Address of GatewayService RPC")
        <*> strOption (long "gateway-pub-endpoint"
                       <> metavar "PUB_ENDPOINT"
                       <> help "Address of Gateway Pub stream")
        <*> strOption (long "ephemeral-rpc-endpoint"
                       <> metavar "RPC_ENDPOINT"
                       <> help "Address of EphemeralService RPC")
        <*> strOption (long "server"
                       <> metavar "SERVER"
                       <> help "Address of IRC server to join")
        <*> strOption (long "nickname"
                       <> metavar "NICK"
                       <> help "Bot nickname")
        <*> strOption (long "channels"
                       <> metavar "CHANNEL1,CHANNEL2,..."
                       <> help "Comma-separated list of IRC channels to join")

flagsOpts = info (helper <*> flags)
            ( fullDesc
              <> progDesc "Run gateway"
              <> header "gateway - klacz irc frontend" )

main :: IO ()
main = execParser flagsOpts >>= \flags -> withSocketsDo $ do
  addrs <- hostAddresses <$> getHostByName (flagIrcServer flags)
  when (null addrs) $ error "unknown address"
  sock <- socket AF_INET Stream 0
  connect sock (SockAddrInet (toEnum 6667) (head addrs))
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering
  start flags  handle
  s <- atomically $ takeTMVar childDiesTMVar
  putStrLn $ "Child died: " ++ s
  return ()
