module Ephemeral.Client(getValue, setValue, deleteValue, clear) where

import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Network.RPC.Protopap.Client
import System.ZMQ4

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

logErr err = liftIO (putStrLn $ "EphemeralClient: got RPC error " ++ show err)

getValue :: MonadIO m => Socket Req -> T.Text -> m (Maybe LBS.ByteString)
getValue sock key = do
  res <- rpcCall sock "GetValue" gvMsg
  case res of
    Left err -> logErr err >> return Nothing
    Right (GetValueResponse { GetValueResponse.value = v }) -> return v
  where gvMsg = GetValueRequest { GetValueRequest.key = Just . uFromText $ key }

setValue :: MonadIO m => Socket Req -> T.Text -> LBS.ByteString -> m ()
setValue sock key value = do
  res <- rpcCall sock "SetValue" svMsg
  case res of
    Left err -> logErr err >> return ()
    Right (SetValueResponse {}) -> return ()
  where svMsg = SetValueRequest { SetValueRequest.key = Just . uFromText $ key,
                                  SetValueRequest.value = Just value }

deleteValue :: MonadIO m => Socket Req -> T.Text -> m ()
deleteValue sock key = do
  res <- rpcCall sock "DeleteValue" dvMsg
  case res of
    Left err -> logErr err >> return ()
    Right (DeleteValueResponse {}) -> return ()
  where dvMsg = DeleteValueRequest { DeleteValueRequest.key = Just . uFromText $ key }

clear :: MonadIO m => Socket Req -> m ()
clear sock = do
  res <- rpcCall sock "Clear" cMsg
  case res of
    Left err -> logErr err >> return ()
    Right (ClearResponse {}) -> return ()
  where cMsg = ClearRequest { ClearRequest.ignore = Nothing }
