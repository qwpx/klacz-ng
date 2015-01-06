{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module IRC.UserCommand (UserCommand(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data UserCommand = UserCommand{caller :: !(P'.Maybe P'.Utf8), replyTo :: !(P'.Maybe P'.Utf8), command :: !(P'.Maybe P'.Utf8),
                               args :: !(P'.Maybe P'.Utf8)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable UserCommand where
  mergeAppend (UserCommand x'1 x'2 x'3 x'4) (UserCommand y'1 y'2 y'3 y'4)
   = UserCommand (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default UserCommand where
  defaultValue = UserCommand P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire UserCommand where
  wireSize ft' self'@(UserCommand x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 9 x'3 + P'.wireSizeOpt 1 9 x'4)
  wirePut ft' self'@(UserCommand x'1 x'2 x'3 x'4)
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
             P'.wirePutOpt 26 9 x'3
             P'.wirePutOpt 34 9 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{caller = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{replyTo = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{command = Prelude'.Just new'Field}) (P'.wireGet 9)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{args = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> UserCommand) UserCommand where
  getVal m' f' = f' m'
 
instance P'.GPB UserCommand
 
instance P'.ReflectDescriptor UserCommand where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".IRC.UserCommand\", haskellPrefix = [], parentModule = [MName \"IRC\"], baseName = MName \"UserCommand\"}, descFilePath = [\"IRC\",\"UserCommand.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".IRC.UserCommand.caller\", haskellPrefix' = [], parentModule' = [MName \"IRC\",MName \"UserCommand\"], baseName' = FName \"caller\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".IRC.UserCommand.replyTo\", haskellPrefix' = [], parentModule' = [MName \"IRC\",MName \"UserCommand\"], baseName' = FName \"replyTo\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".IRC.UserCommand.command\", haskellPrefix' = [], parentModule' = [MName \"IRC\",MName \"UserCommand\"], baseName' = FName \"command\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".IRC.UserCommand.args\", haskellPrefix' = [], parentModule' = [MName \"IRC\",MName \"UserCommand\"], baseName' = FName \"args\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType UserCommand where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg UserCommand where
  textPut msg
   = do
       P'.tellT "caller" (caller msg)
       P'.tellT "replyTo" (replyTo msg)
       P'.tellT "command" (command msg)
       P'.tellT "args" (args msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'caller, parse'replyTo, parse'command, parse'args]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'caller
         = P'.try
            (do
               v <- P'.getT "caller"
               Prelude'.return (\ o -> o{caller = v}))
        parse'replyTo
         = P'.try
            (do
               v <- P'.getT "replyTo"
               Prelude'.return (\ o -> o{replyTo = v}))
        parse'command
         = P'.try
            (do
               v <- P'.getT "command"
               Prelude'.return (\ o -> o{command = v}))
        parse'args
         = P'.try
            (do
               v <- P'.getT "args"
               Prelude'.return (\ o -> o{args = v}))