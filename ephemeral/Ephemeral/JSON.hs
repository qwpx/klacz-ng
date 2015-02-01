module Ephemeral.JSON(getEphemeral, setEphemeral) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import Data.Aeson
import qualified Data.Text as T

import System.ZMQ4

import Ephemeral.Client

getEphemeral :: (MonadIO m, FromJSON a) => Socket Req -> T.Text -> m (Maybe a)
getEphemeral sock key =  do
  val <- getValue sock key
  return $ head <$> (join $ decode <$> val)

setEphemeral :: (MonadIO m, ToJSON a) => Socket Req -> T.Text -> a -> m ()
setEphemeral sock key value = do
  setValue sock key $ encode [value]
