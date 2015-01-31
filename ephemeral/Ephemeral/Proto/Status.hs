{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Ephemeral.Proto.Status (Status(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Status = OK
            | NOT_FOUND
            | INTERNAL_ERROR
            | UNKNOWN
            deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Status
 
instance Prelude'.Bounded Status where
  minBound = OK
  maxBound = UNKNOWN
 
instance P'.Default Status where
  defaultValue = OK
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Status
toMaybe'Enum 1 = Prelude'.Just OK
toMaybe'Enum 2 = Prelude'.Just NOT_FOUND
toMaybe'Enum 3 = Prelude'.Just INTERNAL_ERROR
toMaybe'Enum 4 = Prelude'.Just UNKNOWN
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Status where
  fromEnum OK = 1
  fromEnum NOT_FOUND = 2
  fromEnum INTERNAL_ERROR = 3
  fromEnum UNKNOWN = 4
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Ephemeral.Proto.Status") . toMaybe'Enum
  succ OK = NOT_FOUND
  succ NOT_FOUND = INTERNAL_ERROR
  succ INTERNAL_ERROR = UNKNOWN
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Ephemeral.Proto.Status"
  pred NOT_FOUND = OK
  pred INTERNAL_ERROR = NOT_FOUND
  pred UNKNOWN = INTERNAL_ERROR
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Ephemeral.Proto.Status"
 
instance P'.Wire Status where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Status
 
instance P'.MessageAPI msg' (msg' -> Status) Status where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Status where
  reflectEnum = [(1, "OK", OK), (2, "NOT_FOUND", NOT_FOUND), (3, "INTERNAL_ERROR", INTERNAL_ERROR), (4, "UNKNOWN", UNKNOWN)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Ephemeral.Proto.Status") [] ["Ephemeral", "Proto"] "Status")
      ["Ephemeral", "Proto", "Status.hs"]
      [(1, "OK"), (2, "NOT_FOUND"), (3, "INTERNAL_ERROR"), (4, "UNKNOWN")]
 
instance P'.TextType Status where
  tellT = P'.tellShow
  getT = P'.getRead