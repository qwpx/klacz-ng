module KlaczNG.Helpers(textFromBS, textFromLBS,
                       textToBS, textToLBS,
                       stringToBS, stringToLBS,
                       uFromText, uToText,
                       uFromByteString) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Text.ProtocolBuffers.Basic

textFromBS :: BS.ByteString -> Text
textFromBS = decodeUtf8With lenientDecode

textFromLBS :: LBS.ByteString -> Text
textFromLBS = decodeUtf8With lenientDecode . LBS.toStrict

textToBS :: Text -> BS.ByteString
textToBS = encodeUtf8

textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromStrict . encodeUtf8

stringToBS :: String -> BS.ByteString
stringToBS = textToBS . pack

stringToLBS :: String -> LBS.ByteString
stringToLBS = textToLBS . pack

uFromText :: Text -> Utf8
uFromText = uFromString . unpack

uToText :: Utf8 -> Text
uToText = textFromLBS . utf8

uFromByteString :: BS.ByteString -> Utf8
uFromByteString = uFromText . textFromBS
