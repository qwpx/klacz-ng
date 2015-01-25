{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module KlaczNG.IRC.Proto.SendGatewayMessageRequest (SendGatewayMessageRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified KlaczNG.IRC.Proto.IrcMessage as KlaczNG.IRC.Proto (IrcMessage)
 
data SendGatewayMessageRequest = SendGatewayMessageRequest{irc_message :: !(P'.Maybe KlaczNG.IRC.Proto.IrcMessage)}
                               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable SendGatewayMessageRequest where
  mergeAppend (SendGatewayMessageRequest x'1) (SendGatewayMessageRequest y'1) = SendGatewayMessageRequest (P'.mergeAppend x'1 y'1)
 
instance P'.Default SendGatewayMessageRequest where
  defaultValue = SendGatewayMessageRequest P'.defaultValue
 
instance P'.Wire SendGatewayMessageRequest where
  wireSize ft' self'@(SendGatewayMessageRequest x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1)
  wirePut ft' self'@(SendGatewayMessageRequest x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{irc_message = P'.mergeAppend (irc_message old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> SendGatewayMessageRequest) SendGatewayMessageRequest where
  getVal m' f' = f' m'
 
instance P'.GPB SendGatewayMessageRequest
 
instance P'.ReflectDescriptor SendGatewayMessageRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".KlaczNG.IRC.Proto.SendGatewayMessageRequest\", haskellPrefix = [], parentModule = [MName \"KlaczNG\",MName \"IRC\",MName \"Proto\"], baseName = MName \"SendGatewayMessageRequest\"}, descFilePath = [\"KlaczNG\",\"IRC\",\"Proto\",\"SendGatewayMessageRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".KlaczNG.IRC.Proto.SendGatewayMessageRequest.irc_message\", haskellPrefix' = [], parentModule' = [MName \"KlaczNG\",MName \"IRC\",MName \"Proto\",MName \"SendGatewayMessageRequest\"], baseName' = FName \"irc_message\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".KlaczNG.IRC.Proto.IrcMessage\", haskellPrefix = [], parentModule = [MName \"KlaczNG\",MName \"IRC\",MName \"Proto\"], baseName = MName \"IrcMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType SendGatewayMessageRequest where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg SendGatewayMessageRequest where
  textPut msg
   = do
       P'.tellT "irc_message" (irc_message msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'irc_message]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'irc_message
         = P'.try
            (do
               v <- P'.getT "irc_message"
               Prelude'.return (\ o -> o{irc_message = v}))