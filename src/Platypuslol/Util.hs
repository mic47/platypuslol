module Platypuslol.Util
  ( urlEncodeQuery
  , urlEncodeText
  , urlDecodeText
  ) where

import Network.HTTP.Types (urlEncode, urlDecode)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

urlEncodeText :: Text -> Text
urlEncodeText = decodeUtf8 . urlEncode False . encodeUtf8

urlDecodeText :: Text -> Text
urlDecodeText = decodeUtf8 . urlDecode False . encodeUtf8

urlEncodeQuery :: [Text] -> Text
urlEncodeQuery = urlEncodeText . Text.intercalate " "
