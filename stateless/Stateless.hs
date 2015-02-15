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
import Data.Either
import Data.Foldable(toList)
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Sequence (fromList)
import Data.Ord
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
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

import Persistent.Proto.Term
import Persistent.Proto.Term as Term
import Persistent.Proto.Entry
import Persistent.Proto.Entry as Entry
import Persistent.Proto.CreateTermRequest
import Persistent.Proto.CreateTermRequest as CreateTermRequest
import Persistent.Proto.CreateTermResponse
import Persistent.Proto.CreateTermResponse as CreateTermResponse
import Persistent.Proto.GetTermRequest
import Persistent.Proto.GetTermRequest as GetTermRequest
import Persistent.Proto.GetTermResponse
import Persistent.Proto.GetTermResponse as GetTermResponse
import Persistent.Proto.CreateEntryRequest
import Persistent.Proto.CreateEntryRequest as CreateEntryRequest
import Persistent.Proto.CreateEntryResponse
import Persistent.Proto.CreateEntryResponse as CreateEntryResponse
import Persistent.Proto.GetEntriesRequest
import Persistent.Proto.GetEntriesRequest as GetEntriesRequest
import Persistent.Proto.GetEntriesResponse
import Persistent.Proto.GetEntriesResponse as GetEntriesResponse

data Flags = Flags {
  gatewayRpcEndpoint :: String,
  gatewayPubEndpoint :: String,
  ephemeralRpcEndpoint :: String,
  persistentRpcEndpoint :: String
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
                       <> help "Address of Ephemeral RPC")
        <*> strOption (long "persistent-rpc-endpoint"
                       <> metavar "RPC_ENDPOINT"
                       <> help "Address of Persistent RPC")

flagsOpts = info (helper <*> flags)
            ( fullDesc
              <> progDesc "Run module"
              <> header "stateless - klacz module providing statless bot functions" )

main :: IO ()
main = execParser flagsOpts >>= \flags ->
  subscribe (gatewayRpcEndpoint flags) (gatewayPubEndpoint flags) (ephemeralRpcEndpoint flags) (persistentRpcEndpoint flags)

subscribe :: String -> String -> String -> String -> IO ()
subscribe gatewayRpcEndpoint gatewayPubEndpoint ephemeralRpcEndpoint persistentRpcEndpoint = do
  bracket create destroy $ \(_, gatewayRpcSock, gatewayPubSock,
                             ephemeralRpcSock, persistentRpcSock) -> do
    runStateless (subscriberLoop gatewayPubSock) (StatelessEnv gatewayRpcSock ephemeralRpcSock persistentRpcSock)
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
          persistentRpcSock <- ZMQ.socket ctx ZMQ.Req
          ZMQ.connect persistentRpcSock persistentRpcEndpoint
          return (ctx,
                  gatewayRpcSock, gatewayPubSock,
                  ephemeralRpcSock, persistentRpcSock)
        destroy (ctx,
                 gatewayRpcSock, gatewayPubSock,
                 ephemeralRpcSock, persistentRpcSock) = do
          ZMQ.close gatewayRpcSock
          ZMQ.close gatewayPubSock
          ZMQ.close ephemeralRpcSock
          ZMQ.close persistentRpcSock
          ZMQ.term ctx


data StatelessEnv = StatelessEnv {
  gatewayRPCSock :: ZMQ.Socket ZMQ.Req,
  ephemeralRPCSock :: ZMQ.Socket ZMQ.Req,
  persistentRPCSock :: ZMQ.Socket ZMQ.Req
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

replyLinesMax = 4

replyMultiline :: Text -> [Text] -> Stateless ()
replyMultiline replyTo lines = do
  let (now, later) = splitAt replyLinesMax lines
  mapM_ (reply replyTo) now
  unless (null later) $ do
    reply replyTo $ "But wait, there's more! " <>
      "(" <> (T.pack . show . length $ later) <> " more, type ,more)"
  sock <- ephemeralRPCSock <$> ask
  setEphemeral sock "more" later

parseArgs :: Int -> T.Text -> [T.Text]
parseArgs n t = parseArgs' n (T.strip t)
  where parseArgs' 1 t = [t]
        parseArgs' n t = arg : parseArgs (n-1) rest
          where (arg, rest) = T.breakOn " " t

type StatelessCommand = Text -> Text -> Text -> Stateless ()
commands :: M.Map Text StatelessCommand
commands = M.fromList [
  ("more", moreCommand),
  ("pick", pickCommand),
  ("sage", sageCommand),
  ("add", addCommand),
  ("describe", describeCommand)
  ]

moreCommand :: Text -> Text -> Text -> Stateless ()
moreCommand replyTo caller args = do
  sock <- ephemeralRPCSock <$> ask
  moreLines <- getEphemeral sock "more"
  case moreLines of
    Nothing -> reply replyTo "No more lines."
    Just [] -> reply replyTo "No more lines."
    Just moreLines -> replyMultiline replyTo moreLines


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
          botNickname <- fromMaybe (error "expected nickname in ephemeral")
                         <$> getEphemeral sock "nickname"
          let (nick', reason') = if ICU.toLower ICU.Current nick ==
                                    ICU.toLower ICU.Current botNickname
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

-- these should probably be moved to Persistent
getTerm :: Text -> Stateless (Either RPCCallError (Maybe Term))
getTerm termName = do
  sock <- persistentRPCSock <$> ask
  res <- rpcCall sock "GetTerm" $ GetTermRequest {
    GetTermRequest.name = Just . uFromText $ termName }
  case res of
    Left err -> return (Left err)
    Right app_res -> return . Right . GetTermResponse.term $ app_res

createTerm :: Text -> Stateless (Either RPCCallError Term)
createTerm termName = do
  sock <- persistentRPCSock <$> ask
  res <- rpcCall sock "CreateTerm" $ CreateTermRequest {
    CreateTermRequest.name = Just $ uFromText termName }
  case res of
    Left err -> return $ Left err
    Right app_res -> return . Right $
                     fromMaybe (error "expected term in CreateTermResponse")
                     (CreateTermResponse.term app_res)

getOrCreateTerm :: Text -> Stateless (Either RPCCallError Term)
getOrCreateTerm termName = do
  res <- getTerm termName
  case res of
    Left err -> return $ Left err
    Right (Just t) -> return $ Right t
    Right Nothing -> createTerm termName

addEntryToTerm :: Term -> Text -> Text -> Stateless (Either RPCCallError Entry)
addEntryToTerm term author entry = do
  sock <- persistentRPCSock <$> ask
  res <- rpcCall sock "CreateEntry" $ CreateEntryRequest {
    CreateEntryRequest.term = Just $ term,
    CreateEntryRequest.author = Just $ uFromText author,
    CreateEntryRequest.text = Just $ uFromText entry }
  return (fromMaybe (error "expected entry in CreateEntryResponse")
          . CreateEntryResponse.entry <$> res)

getEntries :: Term -> Stateless (Either RPCCallError [Entry])
getEntries term = do
  sock <- persistentRPCSock <$> ask
  res <- rpcCall sock "GetEntries" $ GetEntriesRequest {
    GetEntriesRequest.term = Just term }
  return (toList . GetEntriesResponse.entries <$> res)

addCommand :: Text -> Text -> Text -> Stateless ()
addCommand replyTo caller args = do
  case parseArgs 2 args of
    (termName:entry:_) -> newEntry termName entry
    _ -> reply replyTo "syntax error, expected ,add TERM ENTRY..."
  where newEntry termName entry = do
          term <- getOrCreateTerm termName
          case term of
            Left err -> reply replyTo $
                        "Error while getting term: " <> T.pack (show err)
            Right term' -> do
              res' <- addEntryToTerm term' caller entry
              case res' of
                Left err -> reply replyTo $
                            "Error while adding entry: " <> T.pack (show err)
                Right _ -> reply replyTo $
                           "Added entry to term " <> termName

describeCommand :: Text -> Text -> Text -> Stateless ()
describeCommand replyTo caller args = do
  case parseArgs 1 args of
    (termName:_) -> describe termName
    _ -> reply replyTo "syntax error, expected ,describe TERM"
  where describe termName = do
          term <- getTerm termName
          case term of
            Left err -> reply replyTo $
                        "Error while getting term: " <> T.pack (show err)
            Right Nothing -> reply replyTo $ "No such term: " <> termName
            Right (Just term') -> do
              entries <- getEntries term'
              case entries of
                Left err -> reply replyTo $
                            "Error while getting entries: " <> T.pack (show err)
                Right entries' -> replyMultiline replyTo (prepareEntries entries')
        prepareEntries entries = do
         zipWith prepareEntry [0..] entries
        prepareEntry n entry = ("[" <> T.pack (show n) <> "] " <>
                                (uToText $
                                 fromMaybe (error "expected text in entry")
                                 (Entry.text entry)))



