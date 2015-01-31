{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module KlaczNG.Ephemeral.Proto.GetValueRequest (GetValueRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data GetValueRequest = GetValueRequest{key :: !(P'.Maybe P'.Utf8)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable GetValueRequest where
  mergeAppend (GetValueRequest x'1) (GetValueRequest y'1) = GetValueRequest (P'.mergeAppend x'1 y'1)
 
instance P'.Default GetValueRequest where
  defaultValue = GetValueRequest P'.defaultValue
 
instance P'.Wire GetValueRequest where
  wireSize ft' self'@(GetValueRequest x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1)
  wirePut ft' self'@(GetValueRequest x'1)
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
 
instance P'.MessageAPI msg' (msg' -> GetValueRequest) GetValueRequest where
  getVal m' f' = f' m'
 
instance P'.GPB GetValueRequest
 
instance P'.ReflectDescriptor GetValueRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".KlaczNG.Ephemeral.Proto.GetValueRequest\", haskellPrefix = [], parentModule = [MName \"KlaczNG\",MName \"Ephemeral\",MName \"Proto\"], baseName = MName \"GetValueRequest\"}, descFilePath = [\"KlaczNG\",\"Ephemeral\",\"Proto\",\"GetValueRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".KlaczNG.Ephemeral.Proto.GetValueRequest.key\", haskellPrefix' = [], parentModule' = [MName \"KlaczNG\",MName \"Ephemeral\",MName \"Proto\",MName \"GetValueRequest\"], baseName' = FName \"key\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType GetValueRequest where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg GetValueRequest where
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