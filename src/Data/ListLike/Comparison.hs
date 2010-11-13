module Data.ListLike.Comparison (maxMatchLength) where
import qualified Data.ListLike as LL

-- | Calculate the maxium continuous matching length between two sequences.
maxMatchLength :: (LL.ListLike full a, Eq a)
               => full -- ^ Parent String 
               -> full -- ^ Target
               -> Int  -- ^ maximum length
maxMatchLength s1 s2 = maximum $ map (length . takeWhile id . LL.zipWith (==) s1) (LL.tails s2)
