{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module IRC.IrcMessage (IrcMessage(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data IrcMessage = IrcMessage{command :: !(P'.Maybe P'.Utf8), prefix :: !(P'.Maybe P'.Utf8), params :: !(P'.Seq P'.Utf8)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable IrcMessage where
  mergeAppend (IrcMessage x'1 x'2 x'3) (IrcMessage y'1 y'2 y'3)
   = IrcMessage (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default IrcMessage where
  defaultValue = IrcMessage P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire IrcMessage where
  wireSize ft' self'@(IrcMessage x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeRep 1 9 x'3)
  wirePut ft' self'@(IrcMessage x'1 x'2 x'3)
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
             P'.wirePutOpt 18 9 x'2
             P'.wirePutRep 26 9 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{command = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{prefix = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{params = P'.append (params old'Self) new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> IrcMessage) IrcMessage where
  getVal m' f' = f' m'
 
instance P'.GPB IrcMessage
 
instance P'.ReflectDescriptor IrcMessage where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".IRC.IrcMessage\", haskellPrefix = [], parentModule = [MName \"IRC\"], baseName = MName \"IrcMessage\"}, descFilePath = [\"IRC\",\"IrcMessage.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".IRC.IrcMessage.command\", haskellPrefix' = [], parentModule' = [MName \"IRC\",MName \"IrcMessage\"], baseName' = FName \"command\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".IRC.IrcMessage.prefix\", haskellPrefix' = [], parentModule' = [MName \"IRC\",MName \"IrcMessage\"], baseName' = FName \"prefix\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".IRC.IrcMessage.params\", haskellPrefix' = [], parentModule' = [MName \"IRC\",MName \"IrcMessage\"], baseName' = FName \"params\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType IrcMessage where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg IrcMessage where
  textPut msg
   = do
       P'.tellT "command" (command msg)
       P'.tellT "prefix" (prefix msg)
       P'.tellT "params" (params msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'command, parse'prefix, parse'params]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'command
         = P'.try
            (do
               v <- P'.getT "command"
               Prelude'.return (\ o -> o{command = v}))
        parse'prefix
         = P'.try
            (do
               v <- P'.getT "prefix"
               Prelude'.return (\ o -> o{prefix = v}))
        parse'params
         = P'.try
            (do
               v <- P'.getT "params"
               Prelude'.return (\ o -> o{params = P'.append (params o) v}))