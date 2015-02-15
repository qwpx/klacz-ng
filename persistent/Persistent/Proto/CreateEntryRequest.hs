{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Persistent.Proto.CreateEntryRequest (CreateEntryRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Persistent.Proto.Term as Persistent.Proto (Term)
 
data CreateEntryRequest = CreateEntryRequest{term :: !(P'.Maybe Persistent.Proto.Term), author :: !(P'.Maybe P'.Utf8),
                                             text :: !(P'.Maybe P'.Utf8)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable CreateEntryRequest where
  mergeAppend (CreateEntryRequest x'1 x'2 x'3) (CreateEntryRequest y'1 y'2 y'3)
   = CreateEntryRequest (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default CreateEntryRequest where
  defaultValue = CreateEntryRequest P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire CreateEntryRequest where
  wireSize ft' self'@(CreateEntryRequest x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 9 x'3)
  wirePut ft' self'@(CreateEntryRequest x'1 x'2 x'3)
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
             P'.wirePutOpt 18 9 x'2
             P'.wirePutOpt 26 9 x'3
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
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{author = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{text = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> CreateEntryRequest) CreateEntryRequest where
  getVal m' f' = f' m'
 
instance P'.GPB CreateEntryRequest
 
instance P'.ReflectDescriptor CreateEntryRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Persistent.Proto.CreateEntryRequest\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"CreateEntryRequest\"}, descFilePath = [\"Persistent\",\"Proto\",\"CreateEntryRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Persistent.Proto.CreateEntryRequest.term\", haskellPrefix' = [], parentModule' = [MName \"Persistent\",MName \"Proto\",MName \"CreateEntryRequest\"], baseName' = FName \"term\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Persistent.Proto.Term\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"Term\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Persistent.Proto.CreateEntryRequest.author\", haskellPrefix' = [], parentModule' = [MName \"Persistent\",MName \"Proto\",MName \"CreateEntryRequest\"], baseName' = FName \"author\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Persistent.Proto.CreateEntryRequest.text\", haskellPrefix' = [], parentModule' = [MName \"Persistent\",MName \"Proto\",MName \"CreateEntryRequest\"], baseName' = FName \"text\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType CreateEntryRequest where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg CreateEntryRequest where
  textPut msg
   = do
       P'.tellT "term" (term msg)
       P'.tellT "author" (author msg)
       P'.tellT "text" (text msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'term, parse'author, parse'text]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'term
         = P'.try
            (do
               v <- P'.getT "term"
               Prelude'.return (\ o -> o{term = v}))
        parse'author
         = P'.try
            (do
               v <- P'.getT "author"
               Prelude'.return (\ o -> o{author = v}))
        parse'text
         = P'.try
            (do
               v <- P'.getT "text"
               Prelude'.return (\ o -> o{text = v}))