module Main where

import Control.Monad.Trans.State

import qualified Data.HashMap.Strict as HM

import Network.RPC.Protopap.Server

import KlaczNG.Ephemeral.Proto.GetValueRequest
import KlaczNG.Ephemeral.Proto.GetValueResponse
import KlaczNG.Ephemeral.Proto.SetValueRequest
import KlaczNG.Ephemeral.Proto.SetValueResponse
import KlaczNG.Ephemeral.Proto.DeleteValueRequest
import KlaczNG.Ephemeral.Proto.DeleteValueResponse
import KlaczNG.Ephemeral.Proto.ClearRequest
import KlaczNG.Ephemeral.Proto.ClearResponse
import KlaczNG.Helpers



main = putStrLn "Hello"
