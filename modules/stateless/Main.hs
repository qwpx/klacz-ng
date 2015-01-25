{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell, MultiParamTypeClasses, TypeFamilies, RankNTypes #-}
module Main where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception.Lifted
import Control.Monad as M
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Crypto.Hash.SHA1 as CH

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

import Text.ProtocolBuffers.Basic (Utf8, utf8, toUtf8, defaultValue, uFromString)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)

import System.Environment
import qualified System.ZMQ4 as ZMQ

import KlaczNG.IRC.Proto.IrcMessage
import KlaczNG.IRC.Proto.IrcMessage as IM
import KlaczNG.IRC.Proto.UserCommand
import KlaczNG.IRC.Proto.UserCommand as UC
import KlaczNG.IRC.Proto.SendGatewayMessageRequest 
import KlaczNG.IRC.Proto.SendGatewayMessageResponse
import KlaczNG.Helpers

import Network.RPC.Protopap.Subscriber

data StatelessEnv = StatelessEnv {
  gatewayRPCEndpoint :: String
  }


newtype Stateless a = Stateless {
  unStateless :: ReaderT StatelessEnv IO a
  } deriving (Functor, Applicative, Monad, MonadBase IO, MonadIO)

runStateless = runReaderT . unStateless

instance MonadReader Stateless where
  type EnvType Stateless = StatelessEnv
  ask = Stateless $ ask
  local f = Stateless . local f . unStateless

instance MonadBaseControl IO Stateless where
  type StM Stateless a = a
  liftBaseWith m = do
    r <- (ask :: Stateless StatelessEnv)
    liftIO $ m (runInBaseWith r)
    where runInBaseWith :: StatelessEnv -> (forall a. Stateless a -> IO a)
          runInBaseWith r m' = runStateless m' r
  restoreM a = return a


instance ZMQRPCClient Stateless where
  withConnectedSocket m = do
    endpoint <- gatewayRPCEndpoint <$> ask
    bracket (create endpoint) destroy $ \(_, sock) -> m sock
    where create endpoint = liftIO $ do
            ctx <- ZMQ.context
            sock <- ZMQ.socket ctx ZMQ.Req
            ZMQ.connect sock endpoint
            return (ctx, sock)
          destroy (ctx, sock) = liftIO $ do
            ZMQ.close sock
            ZMQ.term ctx


sendGatewayMessage :: SendGatewayMessageRequest
                      -> Stateless (Either RPCCallError SendGatewayMessageResponse)
sendGatewayMessage = rpcCall "SendGatewayMessage"

reply :: Text -> Text -> Stateless ()
reply replyTo text = do
  res <- sendGatewayMessage $ SendGatewayMessageRequest (
    Just $ IrcMessage {
       IM.prefix = Nothing,
       IM.command = Just . uFromString $ "PRIVMSG",
       IM.params = fromList $ [uFromText replyTo, uFromText text]
       })
  case res of
    Left callError -> liftIO . Prelude.putStrLn $ "Error during reply: " ++ show callError
    Right _ -> return ()

pick :: Text -> Text
pick = T.intercalate " < " . sortBy (comparing (CH.hash . encodeUtf8)) . T.words

pickCommand :: Text -> Text -> Text -> Stateless ()
pickCommand replyTo caller args = reply replyTo (pick args)

type StatelessCommand = Text -> Text -> Text -> Stateless ()
commands :: M.Map Text StatelessCommand
commands = M.fromList [
  ("pick", pickCommand)
  ]

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


handleIrcMessage :: IrcMessage -> Stateless ()
handleIrcMessage ircMessage = return ()

subscriberDef = makeSubscriberDefinition [
  ("IrcMessage", RPCSubHandler handleIrcMessage),
  ("UserCommand", RPCSubHandler handleUserCommand)
  ]

subscribe pubEndpoint rpcEndpoint = do
  bracket create destroy $ \(_, sock) -> do
    runStateless (subscriberLoop sock) (StatelessEnv rpcEndpoint)
    return ()
  where create = do
          ctx <- ZMQ.context
          sock <- ZMQ.socket ctx ZMQ.Sub
          ZMQ.connect sock pubEndpoint
          ZMQ.subscribe sock ""
          return (ctx, sock)
        destroy (ctx, sock) = do
          ZMQ.close sock
          ZMQ.term ctx

subscriberLoop :: ZMQ.Socket ZMQ.Sub -> Stateless ()
subscriberLoop sock = forever $ do
  res <- rpcHandleSubsciption subscriberDef sock
  case res of
    Left err -> liftIO $ Prelude.putStrLn $ "Got error: " ++ err
    Right () -> return ()

main :: IO ()
main = do
  args <- getArgs
  let (pubEndpoint, rpcEndpoint) = case args of
        (pe:re:_) -> (pe, re)
        _ -> error "too few arguments, usage: stateless zmqPubEndpoint zmqRpcEndpoint"
  subscribe pubEndpoint rpcEndpoint
