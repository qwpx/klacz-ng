{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Control.Exception.Lifted
import Control.Exception.Base(SomeException)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.STM

import Control.Lens

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
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

import Ephemeral.JSON
import qualified Ephemeral.Client as Ephemeral

import KlaczNG.Helpers
import KlaczNG.IRC.Proto.IrcMessage
import KlaczNG.IRC.Proto.IrcMessage as IM
import KlaczNG.IRC.Proto.UserCommand
import KlaczNG.IRC.Proto.UserCommand as UC
import KlaczNG.IRC.Proto.SendGatewayMessageRequest 
import KlaczNG.IRC.Proto.SendGatewayMessageResponse

import Config

bsCrLf = BS.pack [13, 10]
hPutBSCrLf handle bs = BS.hPut handle bs >> BS.hPut handle bsCrLf

data GatewayEnv = GatewayEnv {
  _ircReaderChan :: TChan Message,
  _ircWriterChan :: TChan Message,
  _pubSocket :: ZMQ.Socket ZMQ.Pub,
  _ephemeralSocket :: ZMQ.Socket ZMQ.Req
  }

$(makeLenses ''GatewayEnv)


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
  msg `seq` return ()
  writerChan <- view ircWriterChan
  liftIO . atomically . writeTChan writerChan $ msg

readMessage :: IRCGateway Message
readMessage = do
  readerChan <- view ircReaderChan
  liftIO . atomically . readTChan $ readerChan


joinBotChannels :: IRCGateway ()
joinBotChannels = do
  sock <- view ephemeralSocket
  channels <- fromMaybe (error "expected channels in ephemeral")
              <$> getEphemeral sock "channels"
  mapM_ (sendMessage . joinChan . textToBS) channels

nickservIdentify :: IRCGateway ()
nickservIdentify = do
  sock <- view ephemeralSocket
  pass <- fromMaybe (error "expected nickserv password in ephemeral")
          <$> getEphemeral sock "nickservPass"
  sendMessage (privmsg "NickServ" (textToBS $ "IDENTIFY " <> pass))

onLogin :: Message -> IRCGateway ()
onLogin _ = do
  nickservIdentify
  joinBotChannels

handlePing :: Message -> IRCGateway ()
handlePing pingMessage = case msg_params pingMessage of
  (server:_) -> sendMessage (pong server)
  _ -> error $ "incorrect ping message" ++ show pingMessage

gatewayActions = M.fromList [
  ("001", onLogin),
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
  sock <- view ephemeralSocket
  nickname <- fromMaybe (error "expected nickname in ephemeral")
              <$> getEphemeral sock "nickname"
  case msg_command ircMessage of
    "PRIVMSG" -> maybe (return ()) publishUserCommand $
                 parseCommand nickname ircMessage
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
gateway = forever $ do
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

childDiesTMVar = unsafePerformIO newEmptyTMVarIO

start flags ircConfig handle = do
  readerChan <- atomically $ newTChan
  forkChild "irc reader" $ ircReader handle readerChan
  writerChan <- atomically $ newTChan
  forkChild "irc writer" $ ircWriter handle writerChan

  let gatewayRpcE = flagGatewayRpcEndpoint flags
      gatewayPubE = flagGatewayPubEndpoint flags
      ephemeralRpcE = flagEphemeralRpcEndpoint flags
      nickname = ircNickname ircConfig
      nickname' = textToBS nickname
      channels = ircBotChannels ircConfig
      nickservPass = ircNickServPass ircConfig

  atomically $ writeTChan writerChan (nick nickname')
  atomically $ writeTChan writerChan (user nickname' "8" "*" nickname')

  forkChild "gateway " $
    bracket (create gatewayRpcE gatewayPubE ephemeralRpcE) destroy $
    \(ctx, gatewayRpcSock, gatewayPubSock, ephemeralRpcSock) -> do
      Ephemeral.clear ephemeralRpcSock
      setEphemeral ephemeralRpcSock "nickname" nickname
      setEphemeral ephemeralRpcSock "channels" channels
      setEphemeral ephemeralRpcSock "nickservPass" nickservPass
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
  flagIrcConfig :: String
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
        <*> strOption (long "irc-config"
                       <> metavar "FILE"
                       <> help "File with irc configuration")

flagsOpts = info (helper <*> flags)
            ( fullDesc
              <> progDesc "Run gateway"
              <> header "gateway - klacz irc frontend" )

main :: IO ()
main = execParser flagsOpts >>= \flags -> withSocketsDo $ do
  config <- openFile (flagIrcConfig flags) ReadMode
            >>= (Aeson.eitherDecode <$>) . LBS.hGetContents
  case config of
    Left err -> putStrLn err
    Right config'@(IRCConfig { ircServer = ircServer,
                               ircNickname = ircNickname,
                               ircBotChannels = ircBotChannels }) -> do
      addrs <- hostAddresses <$> getHostByName (T.unpack ircServer)
      when (null addrs) $ error "unknown address"
      sock <- socket AF_INET Stream 0
      connect sock (SockAddrInet (toEnum 6667) (head addrs))
      handle <- socketToHandle sock ReadWriteMode
      hSetBuffering handle LineBuffering
      start flags config' handle
      s <- atomically $ takeTMVar childDiesTMVar
      putStrLn $ "Child died: " ++ s
      return ()
