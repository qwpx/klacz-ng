{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Persistent.Proto.CreateTermResponse (CreateTermResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Persistent.Proto.Term as Persistent.Proto (Term)
 
data CreateTermResponse = CreateTermResponse{term :: !(P'.Maybe Persistent.Proto.Term)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable CreateTermResponse where
  mergeAppend (CreateTermResponse x'1) (CreateTermResponse y'1) = CreateTermResponse (P'.mergeAppend x'1 y'1)
 
instance P'.Default CreateTermResponse where
  defaultValue = CreateTermResponse P'.defaultValue
 
instance P'.Wire CreateTermResponse where
  wireSize ft' self'@(CreateTermResponse x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1)
  wirePut ft' self'@(CreateTermResponse x'1)
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
 
instance P'.MessageAPI msg' (msg' -> CreateTermResponse) CreateTermResponse where
  getVal m' f' = f' m'
 
instance P'.GPB CreateTermResponse
 
instance P'.ReflectDescriptor CreateTermResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Persistent.Proto.CreateTermResponse\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"CreateTermResponse\"}, descFilePath = [\"Persistent\",\"Proto\",\"CreateTermResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Persistent.Proto.CreateTermResponse.term\", haskellPrefix' = [], parentModule' = [MName \"Persistent\",MName \"Proto\",MName \"CreateTermResponse\"], baseName' = FName \"term\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Persistent.Proto.Term\", haskellPrefix = [], parentModule = [MName \"Persistent\",MName \"Proto\"], baseName = MName \"Term\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType CreateTermResponse where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg CreateTermResponse where
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