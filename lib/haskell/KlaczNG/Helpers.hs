module KlaczNG.Helpers(textFromByteString, textFromLazyByteString,
                       uFromText, uToText,
                       uFromByteString) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Text.ProtocolBuffers.Basic

textFromByteString :: BS.ByteString -> Text
textFromByteString = decodeUtf8With lenientDecode

textFromLazyByteString :: LBS.ByteString -> Text
textFromLazyByteString = decodeUtf8With lenientDecode . LBS.toStrict

uFromText :: Text -> Utf8
uFromText = uFromString . unpack

uToText :: Utf8 -> Text
uToText = textFromLazyByteString . utf8

uFromByteString :: BS.ByteString -> Utf8
uFromByteString = uFromText . textFromByteString
