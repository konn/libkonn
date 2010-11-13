{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
module Data.Configuration ( Conf, as, valueForKey, (<@>), keys
                          , (!), toConf, toDyn, toMap) where
import Data.Map hiding ((!), keys)
import qualified Data.Map as M
import Data.Dynamic
import Prelude hiding (lookup)
import Control.Applicative
import Data.Maybe
import Network.OAuth.Consumer (Token(..))
import Data.Generics
import Network.HTTP

deriving instance Typeable1 Request
deriving instance Typeable  Token

newtype Conf = Conf { toMap :: Map String Dynamic }
toConf :: [(String, Dynamic)] -> Conf
toConf = Conf . fromList

as :: a
as = undefined

keys :: Conf -> [String]
keys = M.keys . toMap

valueForKey :: Typeable a => String -> Conf -> a -> Maybe a
valueForKey key conf _ = fromDynamic =<< lookup key (toMap conf)

(!) :: Typeable a => Conf -> String -> a
conf ! key = case valueForKey key conf undefined of
               Just val -> val
               Nothing  -> error $ "no such key: " ++ key

(<@>) :: Typeable a => String -> a -> (String, Dynamic)
key <@> val = (key, toDyn val)

