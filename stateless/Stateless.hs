{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell, MultiParamTypeClasses, TypeFamilies, RankNTypes, ConstraintKinds #-}
module Main where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception.Lifted
import Control.Lens
import Control.Monad as M
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Crypto.Hash.SHA1 as CH

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Sequence (fromList)
import Data.Ord
import Data.Text as T
import Data.Text.Encoding

import Network.RPC.Protopap.Types
import Network.RPC.Protopap.Client
import Network.RPC.Protopap.Subscriber

import Text.ProtocolBuffers.Basic (Utf8, utf8, toUtf8, defaultValue, uFromString)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)

import Options.Applicative

import System.Environment
import qualified System.ZMQ4 as ZMQ

import KlaczNG.IRC.Proto.IrcMessage
import KlaczNG.IRC.Proto.IrcMessage as IM
import KlaczNG.IRC.Proto.UserCommand
import KlaczNG.IRC.Proto.UserCommand as UC
import KlaczNG.IRC.Proto.SendGatewayMessageRequest 
import KlaczNG.IRC.Proto.SendGatewayMessageResponse
import KlaczNG.Helpers

import Ephemeral.JSON

data Flags = Flags {
  gatewayRpcEndpoint :: String,
  gatewayPubEndpoint :: String,
  ephemeralRpcEndpoint :: String
  }

flags = Flags
        <$> strOption (long "gateway-rpc-endpoint"
                       <> metavar "RPC_ENDPOINT"
                       <> help "Address of GatewayService RPC")
        <*> strOption (long "gateway-pub-endpoint"
                       <> metavar "PUB_ENDPOINT"
                       <> help "Address of Gateway Pub stream")
        <*> strOption (long "ephemeral-pub-endpoint"
                       <> metavar "PUB_ENDPOINT"
                       <> help "Address of Ephemeral RPC")

flagsOpts = info (helper <*> flags)
            ( fullDesc
              <> progDesc "Run module"
              <> header "stateless - klacz module providing statless bot functions" )

main :: IO ()
main = execParser flagsOpts >>= \flags ->
  subscribe (gatewayRpcEndpoint flags) (gatewayPubEndpoint flags) (ephemeralRpcEndpoint flags)

subscribe :: String -> String -> String -> IO ()
subscribe gatewayRpcEndpoint gatewayPubEndpoint ephemeralRpcEndpoint = do
  bracket create destroy $ \(_, gatewayRpcSock, gatewayPubSock, ephemeralRpcSock) -> do
    runStateless (subscriberLoop gatewayPubSock) (StatelessEnv gatewayRpcSock ephemeralRpcSock)
    return ()
  where create = do
          ctx <- ZMQ.context
          gatewayRpcSock <- ZMQ.socket ctx ZMQ.Req
          ZMQ.connect gatewayRpcSock gatewayRpcEndpoint
          gatewayPubSock <- ZMQ.socket ctx ZMQ.Sub
          ZMQ.connect gatewayPubSock gatewayPubEndpoint
          ZMQ.subscribe gatewayPubSock ""
          ephemeralRpcSock <- ZMQ.socket ctx ZMQ.Req
          ZMQ.connect ephemeralRpcSock ephemeralRpcEndpoint
          return (ctx, gatewayRpcSock, gatewayPubSock, ephemeralRpcSock)
        destroy (ctx, gatewayRpcSock, gatewayPubSock, ephemeralRpcSock) = do
          ZMQ.close gatewayRpcSock
          ZMQ.close gatewayPubSock
          ZMQ.close ephemeralRpcSock
          ZMQ.term ctx


data StatelessEnv = StatelessEnv {
  gatewayRPCSock :: ZMQ.Socket ZMQ.Req,
  ephemeralRPCSock :: ZMQ.Socket ZMQ.Req
  }

type Stateless a = ReaderT StatelessEnv IO a

runStateless = runReaderT

subscriberLoop :: ZMQ.Socket ZMQ.Sub -> Stateless ()
subscriberLoop sock = forever $ do
  res <- rpcHandleSubsciption subscriberDef sock
  case res of
    Left err -> liftIO $ Prelude.putStrLn $ "Got error: " ++ err
    Right () -> return ()

subscriberDef = makeSubscriberDefinition [
  ("IrcMessage", RPCSubHandler handleIrcMessage),
  ("UserCommand", RPCSubHandler handleUserCommand)
  ]

handleIrcMessage :: IrcMessage -> Stateless ()
handleIrcMessage ircMessage = return ()

handleUserCommand :: UserCommand -> Stateless ()
handleUserCommand  uc@(UserCommand { UC.caller = Just caller,
                                     UC.command = Just command,
                                     UC.replyTo = Just replyTo,
                                     UC.args = Just args }) = do
  liftIO . Prelude.putStrLn $ "Got command: " ++ show uc
  let caller'  = uToText caller
      command' = uToText command
      replyTo' = uToText replyTo
      args'    = uToText args
  case M.lookup command' commands of
    Nothing -> reply replyTo' $ "No such command: " <> command'
    Just f -> f replyTo' caller' args'

sendGatewayMessage :: SendGatewayMessageRequest
                      -> Stateless (Either RPCCallError SendGatewayMessageResponse)
sendGatewayMessage appRequest = do
  sock <- gatewayRPCSock <$> ask
  rpcCall sock "SendGatewayMessage" appRequest

reply :: Text -> Text -> Stateless ()
reply replyTo text = do
  res <- sendGatewayMessage $ SendGatewayMessageRequest (
    Just $ IrcMessage {
       IM.prefix = Nothing,
       IM.command = Just . uFromString $ "PRIVMSG",
       IM.params = fromList $ [uFromText replyTo, uFromText text]})
  case res of
    Left callError -> liftIO . Prelude.putStrLn $ "Error during reply: " ++ show callError
    Right _ -> return ()

parseArgs :: Int -> T.Text -> [T.Text]
parseArgs n t = parseArgs' n (T.strip t)
  where parseArgs' 1 t = [t]
        parseArgs' n t = arg : parseArgs (n-1) rest
          where (arg, rest) = T.breakOn " " t

type StatelessCommand = Text -> Text -> Text -> Stateless ()
commands :: M.Map Text StatelessCommand
commands = M.fromList [
  ("pick", pickCommand),
  ("sage", sageCommand)
  ]

pick :: Text -> Text
pick = T.intercalate " < " . sortBy (comparing (CH.hash . encodeUtf8)) . T.words

pickCommand :: Text -> Text -> Text -> Stateless ()
pickCommand replyTo caller args = reply replyTo (pick args)

sageCommand :: Text -> Text -> Text -> Stateless ()
sageCommand replyTo caller args = do
  case parseArgs 2 args of
    (nick:reason:_) -> sageWithReason nick reason
    (nick:[]) -> sageWithReason nick nick
    [] -> sageWithReason caller caller
  where sageWithReason nick reason = do
          sock <- ephemeralRPCSock <$> ask
          botNickname <- fromMaybe "expected nickname in ephemeral"
                         <$> getEphemeral sock "nickname"
          let (nick', reason') = if nick == botNickname
                                 then (caller, "na pana rękę podnosisz?")
                                 else (nick, reason)
          res <- sendGatewayMessage $ SendGatewayMessageRequest (
            Just $ IrcMessage {
               IM.prefix = Nothing,
               IM.command = Just . uFromString $ "KICK",
               IM.params = fromList $ [uFromText replyTo,
                                       uFromText nick',
                                       uFromText reason']})
          case res of
            Left callError -> liftIO . Prelude.putStrLn $
                              "Error during sage: " ++ show callError
            Right _ -> return ()




