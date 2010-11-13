{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns, RecordWildCards, DisambiguateRecordFields, OverloadedStrings #-}
module Network.OAuthWrapper ( fromList, toList, signOAuth, fromOAReq
                    , toOAReq, Token(..), Application(..), OAuthCallback(..)) where
import Network.OAuth.Consumer
import Network.OAuth.Http.Request (fromList, parseURL, empty, toList, findWithDefault)
import qualified Network.OAuth.Http.Request as OA
import Network.URI
import Data.List
import Control.Arrow hiding (app)
import Data.Maybe
import Network.Browser
import Network.CGI
import Network.HTTP
import Data.ByteString.Lazy.Char8 hiding (intercalate, concat, map, empty, drop, head)
import Data.String
import Data.List.Split
import Control.Applicative hiding (empty)

mtdDic :: [(OA.Method, RequestMethod)]
mtdDic = [(OA.GET, GET), (OA.POST, POST), (OA.PUT, PUT), (OA.DELETE, DELETE), (OA.TRACE, TRACE), (OA.CONNECT, CONNECT), (OA.HEAD, HEAD)]

toOAReq :: Request ByteString -> OA.Request
toOAReq Request{..} = OA.ReqHttp{ OA.version = OA.Http11, ..}
  where
    ssl = uriScheme rqURI == "https"
    method = fromMaybe OA.GET $ lookup rqMethod $ map (snd&&&fst) mtdDic
    pathComps = splitOn "/" $ uriPath rqURI
    qString = fromList $ formDecode $ drop 1 $ uriQuery rqURI
    reqHeaders = fromList $ map (show . hdrName &&& hdrValue) rqHeaders
    reqPayload = rqBody
    host = fromJust (uriRegName <$> uriAuthority rqURI)
    port = fromJust (readWith 80 . drop 1 . uriPort <$> uriAuthority rqURI)

readWith :: Read a => a -> String -> a
readWith def = fst . head .(++[(def, "")]) . reads 

fromOAReq :: OA.Request -> Request ByteString 
fromOAReq OA.ReqHttp{..} = Request { rqURI, rqMethod, rqHeaders, rqBody = reqPayload }
  where
    rqMethod = fromMaybe (Custom "GET") $ lookup method mtdDic
    rqURI :: URI
    rqURI = let protocol = if ssl then "https//:" else "http://"
                uriQuery = if qString == empty then "" else '?':formEncode (toList qString)
            in fromJust $ parseURI $ protocol ++ host ++ ':':show port ++ intercalate "/" pathComps ++ uriQuery
    rqHeaders = map (first HdrCustom >>> uncurry mkHeader) $ toList reqHeaders


signOAuth :: Token -> Request ByteString -> IO (Request ByteString)
signOAuth tok req = runOAuthM tok $ 
  fromOAReq . unpackRq <$> signRq2 HMACSHA1 Nothing (toOAReq req)
