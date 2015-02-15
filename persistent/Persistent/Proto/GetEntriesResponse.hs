{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Persistent.Proto.GetEntriesResponse (GetEntriesResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Persistent.Proto.Entry as Persistent.Proto (Entry)
 
data GetEntriesResponse = GetEntriesResponse{entries :: !(P'.Seq Persistent.Proto.Entry)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable GetEntriesResponse where
  mergeAppend (GetEntriesResponse x'1) (GetEntriesResponse y'1) = GetEntriesResponse (P'.mergeAppend x'1 y'1)
 
instance P'.Default GetEntriesResponse where
  defaultValue = GetEntriesResponse P'.defaultValue
 
instance P'.Wire GetEntriesResponse where
  wireSize ft' self'@(GetEntriesResponse x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(GetEntriesResponse x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{entries = P'.append (entries old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> GetEntriesResponse) GetEntriesResponse where
  getVal m' f' = f' m'
 
instance P'.GPB GetEntriesResponse
 
instance P'.ReflectDescriptor GetEntriesResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Persistent.Proto.GetEntriesResponse\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"GetEntriesResponse\"}, descFilePath = [\"Persistent\",\"Proto\",\"GetEntriesResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Persistent.Proto.GetEntriesResponse.entries\", haskellPrefix' = [], parentModule' = [MName \"Persistent\",MName \"Proto\",MName \"GetEntriesResponse\"], baseName' = FName \"entries\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Persistent.Proto.Entry\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"Entry\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType GetEntriesResponse where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg GetEntriesResponse where
  textPut msg
   = do
       P'.tellT "entries" (entries msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'entries]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'entries
         = P'.try
            (do
               v <- P'.getT "entries"
               Prelude'.return (\ o -> o{entries = P'.append (entries o) v}))