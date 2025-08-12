-- | Simplified charset conversions (UTF-8 only).
module Network.Email.Charset
    ( -- * Charsets
      Charset
    , charsetName
      -- * Lookup
    , charsets
    , lookupCharset
    , defaultCharset
      -- * Conversion
    , fromUnicode
    , toUnicode
    ) where

import           Data.ByteString       (ByteString)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as TE
import qualified Data.Char             as Char

-- | A charset. Only UTF-8 and US-ASCII are supported.
newtype Charset = Charset String
    deriving (Show)

instance Eq Charset where
    a == b = compare a b == EQ

instance Ord Charset where
    compare (Charset a) (Charset b) = 
        compare (map Char.toLower a) (map Char.toLower b)

-- | The name of a charset.
charsetName :: Charset -> String
charsetName (Charset s) = s

-- | Supported charset names and aliases (UTF-8 and US-ASCII only).
charsets :: Set Charset
charsets = Set.fromList 
    [ Charset "UTF-8"
    , Charset "utf-8"
    , Charset "utf8"
    , Charset "UTF8"
    , Charset "US-ASCII"
    , Charset "us-ascii"
    , Charset "ASCII"
    , Charset "ascii"
    ]

-- | Lookup a charset from a name or alias, or 'Nothing' if no such charset
-- exists. Only UTF-8 and US-ASCII are supported.
lookupCharset :: String -> Maybe Charset
lookupCharset name = case Set.lookupLE c charsets of
    Just c' | c' == c -> Just c'
    _                 -> Nothing
  where
    c = Charset name

-- | The default charset, UTF-8.
defaultCharset :: Charset
defaultCharset = Charset "UTF-8"

-- | Convert a Unicode string into a UTF-8 byte string.
fromUnicode :: Charset -> Text -> ByteString
fromUnicode _ = TE.encodeUtf8

-- | Convert a UTF-8 byte string into a Unicode string.
-- For ASCII, we treat it as UTF-8 since ASCII is a subset of UTF-8.
toUnicode :: Charset -> ByteString -> Text
toUnicode (Charset name) bytes = 
    case map Char.toLower name of
        "utf-8"    -> TE.decodeUtf8 bytes
        "utf8"     -> TE.decodeUtf8 bytes
        "us-ascii" -> TE.decodeUtf8 bytes  -- ASCII is subset of UTF-8
        "ascii"    -> TE.decodeUtf8 bytes
        _          -> TE.decodeUtf8 bytes  -- Fallback: treat as UTF-8