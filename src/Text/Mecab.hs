{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
-- |Interface for Mecab.
--  Note that ByteString version is faster than String version.
module Text.Mecab
    ( -- * Word
      Word(..), isNoun
      -- * Word delimit
    , mecab, mecabBS
    , nouns, nounsBS
    ) where
import Data.ByteString hiding (map, filter)
import Data.ByteString.UTF8
import System.Process
import System.IO.Unsafe
import System.IO (hClose, hIsEOF)
import Control.Monad.Loops
import Control.Monad
import Data.Attoparsec
import Data.Attoparsec.Char8
import Data.Maybe
import Prelude hiding (elem, dropWhile)

-- |Data type for Word.
data Word = Word {
      word :: ByteString -- ^ The word itself.
    , hid :: Int         -- ^ Hinshi ID.
    , pid :: Int         -- ^ Word ID.
    } deriving (Show, Eq)

-- |Return True if the word is noun.
isNoun :: Word -> Bool
isNoun Word{hid} = 36 <= hid && hid <= 68

bReadInt :: ByteString -> Maybe Int
bReadInt = maybeResult . flip feed "" . parse decimal

-- |Select nouns from given String.
nouns :: String -> [String]
nouns = map toString . nounsBS . fromString

-- |Devide given paragraph into words.
mecab :: String -> [Word]
mecab = mecabBS . fromString

-- |ByteString version 'nouns'.
nounsBS :: ByteString -> [ByteString]
nounsBS = map word . filter isNoun . mecabBS

-- |ByteString version 'maybe'.
mecabBS :: ByteString -> [Word]
mecabBS str = unsafePerformIO $ do
  (sin, sout, _, ph) <- runInteractiveProcess "mecab" ["--node-format=%M\FS%h\FS%phl\n"] Nothing Nothing
  hPutStrLn sin str
  hClose sin
  cntns <- untilM (hGetLine sout) (hIsEOF sout)
  liftM catMaybes $ forM cntns $ \l -> do
    let (wd:dat) = split 28 l
    case wd of
      ""    -> return Nothing
      "EOS" -> return Nothing
      _     -> case dat of
                 hid:pid:_ -> return $ Just Word{ word= strip wd
                                                , hid = maybe 70 id $ bReadInt hid
                                                , pid = maybe (-1) id $ bReadInt pid}

strip :: ByteString -> ByteString
strip = dropWhile (`elem`" \t\r\n")