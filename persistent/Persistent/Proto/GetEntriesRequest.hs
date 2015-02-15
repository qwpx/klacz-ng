{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Persistent.Proto.GetEntriesRequest (GetEntriesRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Persistent.Proto.Term as Persistent.Proto (Term)
 
data GetEntriesRequest = GetEntriesRequest{term :: !(P'.Maybe Persistent.Proto.Term)}
                       deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable GetEntriesRequest where
  mergeAppend (GetEntriesRequest x'1) (GetEntriesRequest y'1) = GetEntriesRequest (P'.mergeAppend x'1 y'1)
 
instance P'.Default GetEntriesRequest where
  defaultValue = GetEntriesRequest P'.defaultValue
 
instance P'.Wire GetEntriesRequest where
  wireSize ft' self'@(GetEntriesRequest x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1)
  wirePut ft' self'@(GetEntriesRequest x'1)
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{term = P'.mergeAppend (term old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> GetEntriesRequest) GetEntriesRequest where
  getVal m' f' = f' m'
 
instance P'.GPB GetEntriesRequest
 
instance P'.ReflectDescriptor GetEntriesRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Persistent.Proto.GetEntriesRequest\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"GetEntriesRequest\"}, descFilePath = [\"Persistent\",\"Proto\",\"GetEntriesRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Persistent.Proto.GetEntriesRequest.term\", haskellPrefix' = [], parentModule' = [MName \"Persistent\",MName \"Proto\",MName \"GetEntriesRequest\"], baseName' = FName \"term\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Persistent.Proto.Term\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"Term\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType GetEntriesRequest where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg GetEntriesRequest where
  textPut msg
   = do
       P'.tellT "term" (term msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'term]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'term
         = P'.try
            (do
               v <- P'.getT "term"
               Prelude'.return (\ o -> o{term = v}))