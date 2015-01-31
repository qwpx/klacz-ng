{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module KlaczNG.Ephemeral.Proto.DeleteValueRequest (DeleteValueRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data DeleteValueRequest = DeleteValueRequest{key :: !(P'.Maybe P'.Utf8)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable DeleteValueRequest where
  mergeAppend (DeleteValueRequest x'1) (DeleteValueRequest y'1) = DeleteValueRequest (P'.mergeAppend x'1 y'1)
 
instance P'.Default DeleteValueRequest where
  defaultValue = DeleteValueRequest P'.defaultValue
 
instance P'.Wire DeleteValueRequest where
  wireSize ft' self'@(DeleteValueRequest x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1)
  wirePut ft' self'@(DeleteValueRequest x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 9 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{key = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> DeleteValueRequest) DeleteValueRequest where
  getVal m' f' = f' m'
 
instance P'.GPB DeleteValueRequest
 
instance P'.ReflectDescriptor DeleteValueRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".KlaczNG.Ephemeral.Proto.DeleteValueRequest\", haskellPrefix = [], parentModule = [MName \"KlaczNG\",MName \"Ephemeral\",MName \"Proto\"], baseName = MName \"DeleteValueRequest\"}, descFilePath = [\"KlaczNG\",\"Ephemeral\",\"Proto\",\"DeleteValueRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".KlaczNG.Ephemeral.Proto.DeleteValueRequest.key\", haskellPrefix' = [], parentModule' = [MName \"KlaczNG\",MName \"Ephemeral\",MName \"Proto\",MName \"DeleteValueRequest\"], baseName' = FName \"key\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType DeleteValueRequest where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg DeleteValueRequest where
  textPut msg
   = do
       P'.tellT "key" (key msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'key]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'key
         = P'.try
            (do
               v <- P'.getT "key"
               Prelude'.return (\ o -> o{key = v}))