{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module KlaczNG.IRC.Proto.GatewayCommand (GatewayCommand(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data GatewayCommand = GatewayCommand{command :: !(P'.Maybe P'.Utf8), params :: !(P'.Seq P'.Utf8), rest :: !(P'.Maybe P'.Utf8)}
                    deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable GatewayCommand where
  mergeAppend (GatewayCommand x'1 x'2 x'3) (GatewayCommand y'1 y'2 y'3)
   = GatewayCommand (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default GatewayCommand where
  defaultValue = GatewayCommand P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire GatewayCommand where
  wireSize ft' self'@(GatewayCommand x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 9 x'2 + P'.wireSizeOpt 1 9 x'3)
  wirePut ft' self'@(GatewayCommand x'1 x'2 x'3)
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
             P'.wirePutRep 18 9 x'2
             P'.wirePutOpt 26 9 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{command = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{params = P'.append (params old'Self) new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{rest = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> GatewayCommand) GatewayCommand where
  getVal m' f' = f' m'
 
instance P'.GPB GatewayCommand
 
instance P'.ReflectDescriptor GatewayCommand where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".KlaczNG.IRC.Proto.GatewayCommand\", haskellPrefix = [], parentModule = [MName \"KlaczNG\",MName \"IRC\",MName \"Proto\"], baseName = MName \"GatewayCommand\"}, descFilePath = [\"KlaczNG\",\"IRC\",\"Proto\",\"GatewayCommand.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".KlaczNG.IRC.Proto.GatewayCommand.command\", haskellPrefix' = [], parentModule' = [MName \"KlaczNG\",MName \"IRC\",MName \"Proto\",MName \"GatewayCommand\"], baseName' = FName \"command\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".KlaczNG.IRC.Proto.GatewayCommand.params\", haskellPrefix' = [], parentModule' = [MName \"KlaczNG\",MName \"IRC\",MName \"Proto\",MName \"GatewayCommand\"], baseName' = FName \"params\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".KlaczNG.IRC.Proto.GatewayCommand.rest\", haskellPrefix' = [], parentModule' = [MName \"KlaczNG\",MName \"IRC\",MName \"Proto\",MName \"GatewayCommand\"], baseName' = FName \"rest\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType GatewayCommand where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg GatewayCommand where
  textPut msg
   = do
       P'.tellT "command" (command msg)
       P'.tellT "params" (params msg)
       P'.tellT "rest" (rest msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'command, parse'params, parse'rest]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'command
         = P'.try
            (do
               v <- P'.getT "command"
               Prelude'.return (\ o -> o{command = v}))
        parse'params
         = P'.try
            (do
               v <- P'.getT "params"
               Prelude'.return (\ o -> o{params = P'.append (params o) v}))
        parse'rest
         = P'.try
            (do
               v <- P'.getT "rest"
               Prelude'.return (\ o -> o{rest = v}))