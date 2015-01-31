{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module KlaczNG.Ephemeral.Proto.SetValueResponse (SetValueResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified KlaczNG.Ephemeral.Proto.Status as KlaczNG.Ephemeral.Proto (Status)
 
data SetValueResponse = SetValueResponse{status :: !(P'.Maybe KlaczNG.Ephemeral.Proto.Status), status_info :: !(P'.Maybe P'.Utf8)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable SetValueResponse where
  mergeAppend (SetValueResponse x'1 x'2) (SetValueResponse y'1 y'2)
   = SetValueResponse (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default SetValueResponse where
  defaultValue = SetValueResponse P'.defaultValue P'.defaultValue
 
instance P'.Wire SetValueResponse where
  wireSize ft' self'@(SetValueResponse x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 9 x'2)
  wirePut ft' self'@(SetValueResponse x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 14 x'1
             P'.wirePutOpt 18 9 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{status = Prelude'.Just new'Field}) (P'.wireGet 14)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{status_info = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> SetValueResponse) SetValueResponse where
  getVal m' f' = f' m'
 
instance P'.GPB SetValueResponse
 
instance P'.ReflectDescriptor SetValueResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".KlaczNG.Ephemeral.Proto.SetValueResponse\", haskellPrefix = [], parentModule = [MName \"KlaczNG\",MName \"Ephemeral\",MName \"Proto\"], baseName = MName \"SetValueResponse\"}, descFilePath = [\"KlaczNG\",\"Ephemeral\",\"Proto\",\"SetValueResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".KlaczNG.Ephemeral.Proto.SetValueResponse.status\", haskellPrefix' = [], parentModule' = [MName \"KlaczNG\",MName \"Ephemeral\",MName \"Proto\",MName \"SetValueResponse\"], baseName' = FName \"status\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".KlaczNG.Ephemeral.Proto.Status\", haskellPrefix = [], parentModule = [MName \"KlaczNG\",MName \"Ephemeral\",MName \"Proto\"], baseName = MName \"Status\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".KlaczNG.Ephemeral.Proto.SetValueResponse.status_info\", haskellPrefix' = [], parentModule' = [MName \"KlaczNG\",MName \"Ephemeral\",MName \"Proto\",MName \"SetValueResponse\"], baseName' = FName \"status_info\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType SetValueResponse where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg SetValueResponse where
  textPut msg
   = do
       P'.tellT "status" (status msg)
       P'.tellT "status_info" (status_info msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'status, parse'status_info]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'status
         = P'.try
            (do
               v <- P'.getT "status"
               Prelude'.return (\ o -> o{status = v}))
        parse'status_info
         = P'.try
            (do
               v <- P'.getT "status_info"
               Prelude'.return (\ o -> o{status_info = v}))