{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
module Main where

import Control.Exception.Lifted
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM

import Network.RPC.Protopap.Server

import Options.Applicative

import Text.ProtocolBuffers.Basic

import qualified System.ZMQ4 as ZMQ

import Ephemeral.Proto.Status
import Ephemeral.Proto.Status as Status
import Ephemeral.Proto.GetValueRequest
import Ephemeral.Proto.GetValueRequest as GetValueRequest
import Ephemeral.Proto.GetValueResponse
import Ephemeral.Proto.GetValueResponse as GetValueResponse
import Ephemeral.Proto.SetValueRequest
import Ephemeral.Proto.SetValueRequest as SetValueRequest
import Ephemeral.Proto.SetValueResponse
import Ephemeral.Proto.SetValueResponse as SetValueResponse
import Ephemeral.Proto.DeleteValueRequest
import Ephemeral.Proto.DeleteValueRequest as DeleteValueRequest
import Ephemeral.Proto.DeleteValueResponse
import Ephemeral.Proto.DeleteValueResponse as DeleteValueResponse
import Ephemeral.Proto.ClearRequest
import Ephemeral.Proto.ClearRequest as ClearRequest
import Ephemeral.Proto.ClearResponse
import Ephemeral.Proto.ClearResponse as ClearResponse
import KlaczNG.Helpers

data EphemeralState = EphemeralState {
  _kvStore :: HM.HashMap T.Text LBS.ByteString
  }

emptyEphemeralState = EphemeralState (HM.empty)

type Ephemeral a = StateT EphemeralState IO a

$(makeLenses ''EphemeralState)

data Flags = Flags {
  rpcEndpoint :: String
  }

flags = Flags
        <$> strOption (long "rpc-endpoint"
                       <> metavar "RPC_ENDPOINT"
                       <> help "Address of EphemeralService RPC")

flagsOpts = info (helper <*> flags)
            ( fullDesc
              <> progDesc "Run program"
              <> header "ephemeral - simple in-memory non-persistent key-value store" )

main = execParser flagsOpts >>= \(Flags rpcEndpoint) ->
  evalStateT (withRPCServerSocket rpcEndpoint ephemeralRPCServer) emptyEphemeralState

withRPCServerSocket :: MonadBaseControl IO m =>
                       String -> (ZMQ.Socket ZMQ.Rep -> m a) -> m a
withRPCServerSocket endpoint f = liftBaseOp (bracket create destroy) (f . snd)
  where create = do
          ctx <- ZMQ.context
          rpcSock <- ZMQ.socket ctx ZMQ.Rep
          ZMQ.bind rpcSock endpoint
          return (ctx, rpcSock)
        destroy (ctx, rpcSock) = do
          ZMQ.close rpcSock
          ZMQ.term ctx

ephemeralRPCServer :: ZMQ.Socket ZMQ.Rep -> Ephemeral ()
ephemeralRPCServer sock = do
  forever $ handleRPCCall ephemeralServiceDef sock

ephemeralServiceDef = makeServiceDefinition [
  ("GetValue", RPCHandler getValue),
  ("SetValue", RPCHandler setValue),
  ("DeleteValue", RPCHandler deleteValue),
  ("Clear", RPCHandler clear)
  ]


getValue :: GetValueRequest -> Ephemeral (Either String GetValueResponse)
getValue req = case GetValueRequest.key req of
  Nothing -> return . Left $ "No key provided"
  Just k -> do
    map <- use kvStore
    liftIO $ putStrLn (show map)
    case HM.lookup (uToText k) map of
      Nothing -> return . Right $ GetValueResponse {
        GetValueResponse.status = Just Status.NOT_FOUND,
        GetValueResponse.status_info = Nothing,
        GetValueResponse.value = Nothing
        }
      Just v -> return . Right $ GetValueResponse {
        GetValueResponse.status = Just Status.OK,
        GetValueResponse.status_info = Nothing,
        GetValueResponse.value = Just v
        }

setValue :: SetValueRequest -> Ephemeral (Either String SetValueResponse)
setValue req = case (SetValueRequest.key req, SetValueRequest.value req) of
  (Nothing, _) -> return . Left $ "No key provided"
  (_, Nothing) -> return . Left $ "No value provided"
  (Just k, Just v) -> do
    kvStore . at (uToText k) .= Just v
    return . Right $ SetValueResponse {
      SetValueResponse.status = Just Status.OK,
      SetValueResponse.status_info = Nothing
      }

deleteValue :: DeleteValueRequest -> Ephemeral (Either String DeleteValueResponse)
deleteValue req = case DeleteValueRequest.key req of
  Nothing -> return . Left $ "No key provided"
  Just k -> do
    HM.delete (uToText k) <$> use kvStore
    return . Right $ DeleteValueResponse {
      DeleteValueResponse.status = Just Status.OK,
      DeleteValueResponse.status_info = Nothing
      }

clear :: ClearRequest -> Ephemeral (Either String ClearResponse)
clear _ = do
  kvStore .= HM.empty
  return . Right $ ClearResponse {
    ClearResponse.status = Just Status.OK,
    ClearResponse.status_info = Nothing
    }

