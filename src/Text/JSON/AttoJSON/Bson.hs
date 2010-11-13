{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module Text.JSON.AttoJSON.Bson
    ( -- * Convert between JSON and Bson
      toBson, fromBson, toBsonHook
      -- * Additional JSON instances for BSON
    , JSON (..)
    ) where
import Data.Bson
import Data.CompactString.UTF8 (fromByteString_, toByteString)
import Data.Ratio
import Control.Applicative
import Data.Map hiding (mapMaybe)
import qualified Prelude as P
import Prelude
import Data.ByteString.Char8
import Text.JSON.AttoJSON
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import Control.Monad
import Data.Maybe
import System.Locale

instance JSON Document where
    toJSON = JSObject . fromList . P.map (\(k:=v) -> (toByteString k, toJSON v))
    fromJSON (JSObject dic) = mapM (\(k, v) -> (fromByteString_ k :=) <$> fromJSON v) $ toList dic
    fromJSON _              = Nothing

instance JSON Value where
    toJSON (Float a)           = JSNumber $ toRational a
    toJSON (String u)          = JSString $ toByteString u
    toJSON (Doc doc)           = toJSON doc
    toJSON (Array vs)          = JSArray $ P.map toJSON vs
    toJSON (Bin (Binary bin))  = JSString bin
    toJSON (Fun (Function f))  = JSString f
    toJSON (Uuid (UUID uid))   = JSString uid
    toJSON (Md5 (MD5 md5))     = JSString md5
    toJSON (UserDef (UserDefined ud)) = JSString ud
    toJSON (ObjId (Oid w32 w64)) = JSNumber $ toRational $ fromIntegral $ (toInteger w32)*2^64+toInteger w64
    toJSON (Bool b)            = JSBool b
    toJSON (UTC utctime)       = JSString $ pack $ formatTime defaultTimeLocale "%c" utctime 
    toJSON Null                = JSNull
    toJSON (RegEx (Regex a b)) = JSString (toByteString a `append` toByteString b)
    toJSON (JavaScr (Javascript d js)) = JSString $ toByteString js
    toJSON (Sym (Symbol sym))  = JSString . toByteString $ sym
    toJSON (Int32 int)         = JSNumber . toRational . fromIntegral $ int
    toJSON (Int64 int)         = JSNumber . toRational . fromIntegral $ int
    toJSON (Stamp (MongoStamp int)) = JSNumber . toRational . fromIntegral $ int
    toJSON (MinMax mk)         = undefined

    fromJSON JSNull         = Just Null
    fromJSON (JSString str) = Just . String $ fromByteString_ str
    fromJSON (JSNumber num) | denominator num == 1 = Just . Int64 $ floor num
                            | otherwise            = Just . Float $ fromRational num
    fromJSON j@(JSObject _) = Doc <$> fromJSON j
    fromJSON (JSBool b)     = Just $ Bool b
    fromJSON (JSArray jsa)  = Array <$> mapM fromJSON jsa


instance Val ByteString where
    val bs = String (fromByteString_ bs)
    cast' (String str) = Just $ toByteString str
    cast' (JavaScr (Javascript _ str)) = Just $ toByteString str
    cast' (Sym (Symbol str))           = Just $ toByteString str
    cast' (Bin (Binary str))           = Just str
    cast' (Fun (Function str))         = Just str
    cast' _                            = Nothing

-- |Convert JSON into Bson Value. See Also: 'toBsonHook'
toBson :: JSON a => a -> Value
toBson = fromJust . fromJSON . toJSON

-- |Convert Bson into JSON Value.
fromBson :: JSON a => Value -> Maybe a
fromBson = fromJSON . toJSON

-- |Field name specific version 'toBson'.
toBsonHook :: JSON a => [(ByteString, JSValue -> Maybe Value)] -> a -> Value
toBsonHook fs = sub . toJSON
  where
    sub (JSObject dic) = Doc $ 
        mapMaybe (\(k, v) -> (fromByteString_ k :=) <$> ((P.lookup k fs >>= ($v)) <|> (Just . toBson $ v)) ) $ toList dic
    sub a              = toBson a