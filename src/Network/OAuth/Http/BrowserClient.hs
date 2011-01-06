{-# LANGUAGE RecordWildCards, NamedFieldPuns, OverloadedStrings #-}
module Network.OAuth.Http.BrowserClient where
import Network.OAuth.Http.HttpClient
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import qualified Network.HTTP as B
import Data.ByteString.Lazy.Char8 hiding (map, empty, head, drop, intercalate)
import Data.List (intercalate)
import Control.Arrow
import Network.URI
import Network.CGI
import Data.Maybe
import Control.Monad.Trans
import Data.List.Split
import Control.Applicative hiding (empty)

data BrowserClient = BrowserClient

mtdDic :: [(Method, B.RequestMethod)]
mtdDic = [(GET, B.GET), (POST, B.POST), (PUT, B.PUT), (DELETE, B.DELETE), (TRACE, B.TRACE), (CONNECT, B.CONNECT), (HEAD, B.HEAD)]

toOAReq :: B.Request ByteString -> Request
toOAReq B.Request{..} = ReqHttp{ version = Http11, ..}
  where
    ssl = uriScheme rqURI == "https"
    method = fromMaybe GET $ lookup rqMethod $ map (snd&&&fst) mtdDic
    pathComps = splitOn "/" $ uriPath rqURI
    qString = fromList $ formDecode $ drop 1 $ uriQuery rqURI
    reqHeaders = fromList $ map (show . B.hdrName &&& B.hdrValue) rqHeaders
    reqPayload = rqBody
    host = fromJust (uriRegName <$> uriAuthority rqURI)
    port = fromJust (readWith 80 . drop 1 . uriPort <$> uriAuthority rqURI)

readWith :: Read a => a -> String -> a
readWith def = fst . head .(++[(def, "")]) . reads 

fromOAReq :: Request -> B.Request ByteString 
fromOAReq ReqHttp{..} = B.Request { rqURI, rqMethod, rqHeaders, rqBody = reqPayload }
  where
    rqMethod = fromMaybe (B.Custom "GET") $ lookup method mtdDic
    rqURI :: URI
    rqURI = let protocol = if ssl then "https//:" else "http://"
                uriQuery = if (rqMethod /= B.GET || qString == empty)
                           then ""
                           else '?':formEncode (toList qString)
                                
            in fromJust $ parseURI $ protocol ++ host ++ ':':show port ++ intercalate "/" pathComps ++ uriQuery
    rqHeaders = map (first B.HdrCustom >>> uncurry B.mkHeader) $ toList reqHeaders
    rqBody = reqPayload

toOARsp :: B.Response ByteString -> Response
toOARsp B.Response{rspCode = (a,b,c), rspHeaders=hdrs, ..} = RspHttp{..}
  where
    status = a*100+b*10+c
    reason = rspReason
    rspHeaders = fromList $ map toPair hdrs
    toPair :: B.Header -> (String, String)
    toPair (B.Header name val) = (show name, val)
    rspPayload = rspBody
    

instance HttpClient BrowserClient where
   runClient _ req = liftIO $ do
     either (Left . show) (Right . toOARsp) <$> B.simpleHTTP (fromOAReq req)
