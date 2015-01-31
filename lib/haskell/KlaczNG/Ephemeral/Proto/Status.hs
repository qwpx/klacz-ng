{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module KlaczNG.Ephemeral.Proto.Status (Status(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Status = OK
            | NOT_FOUND
            | FAILED_PRECONDITION
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
toMaybe'Enum 3 = Prelude'.Just FAILED_PRECONDITION
toMaybe'Enum 4 = Prelude'.Just INTERNAL_ERROR
toMaybe'Enum 5 = Prelude'.Just UNKNOWN
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Status where
  fromEnum OK = 1
  fromEnum NOT_FOUND = 2
  fromEnum FAILED_PRECONDITION = 3
  fromEnum INTERNAL_ERROR = 4
  fromEnum UNKNOWN = 5
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type KlaczNG.Ephemeral.Proto.Status") . toMaybe'Enum
  succ OK = NOT_FOUND
  succ NOT_FOUND = FAILED_PRECONDITION
  succ FAILED_PRECONDITION = INTERNAL_ERROR
  succ INTERNAL_ERROR = UNKNOWN
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type KlaczNG.Ephemeral.Proto.Status"
  pred NOT_FOUND = OK
  pred FAILED_PRECONDITION = NOT_FOUND
  pred INTERNAL_ERROR = FAILED_PRECONDITION
  pred UNKNOWN = INTERNAL_ERROR
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type KlaczNG.Ephemeral.Proto.Status"
 
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
  reflectEnum
   = [(1, "OK", OK), (2, "NOT_FOUND", NOT_FOUND), (3, "FAILED_PRECONDITION", FAILED_PRECONDITION),
      (4, "INTERNAL_ERROR", INTERNAL_ERROR), (5, "UNKNOWN", UNKNOWN)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".KlaczNG.Ephemeral.Proto.Status") [] ["KlaczNG", "Ephemeral", "Proto"] "Status")
      ["KlaczNG", "Ephemeral", "Proto", "Status.hs"]
      [(1, "OK"), (2, "NOT_FOUND"), (3, "FAILED_PRECONDITION"), (4, "INTERNAL_ERROR"), (5, "UNKNOWN")]
 
instance P'.TextType Status where
  tellT = P'.tellShow
  getT = P'.getRead