{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Persistent.Proto.CreateEntryResponse (CreateEntryResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Persistent.Proto.Entry as Persistent.Proto (Entry)
 
data CreateEntryResponse = CreateEntryResponse{entry :: !(P'.Maybe Persistent.Proto.Entry)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable CreateEntryResponse where
  mergeAppend (CreateEntryResponse x'1) (CreateEntryResponse y'1) = CreateEntryResponse (P'.mergeAppend x'1 y'1)
 
instance P'.Default CreateEntryResponse where
  defaultValue = CreateEntryResponse P'.defaultValue
 
instance P'.Wire CreateEntryResponse where
  wireSize ft' self'@(CreateEntryResponse x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1)
  wirePut ft' self'@(CreateEntryResponse x'1)
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{entry = P'.mergeAppend (entry old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> CreateEntryResponse) CreateEntryResponse where
  getVal m' f' = f' m'
 
instance P'.GPB CreateEntryResponse
 
instance P'.ReflectDescriptor CreateEntryResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Persistent.Proto.CreateEntryResponse\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"CreateEntryResponse\"}, descFilePath = [\"Persistent\",\"Proto\",\"CreateEntryResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Persistent.Proto.CreateEntryResponse.entry\", haskellPrefix' = [], parentModule' = [MName \"Persistent\",MName \"Proto\",MName \"CreateEntryResponse\"], baseName' = FName \"entry\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Persistent.Proto.Entry\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"Entry\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType CreateEntryResponse where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg CreateEntryResponse where
  textPut msg
   = do
       P'.tellT "entry" (entry msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'entry]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'entry
         = P'.try
            (do
               v <- P'.getT "entry"
               Prelude'.return (\ o -> o{entry = v}))