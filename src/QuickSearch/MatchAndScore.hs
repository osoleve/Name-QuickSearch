{-# LANGUAGE ViewPatterns #-}

module QuickSearch.MatchAndScore
  ( scoreMatches
  , Token
  , Score
  , Scorer
  , QuickSearch(QuickSearch)
  )
where

import           Control.Arrow
import           Data.Hashable
import qualified Data.HashMap.Lazy  as HMap
import qualified Data.HashSet       as HSet
import           Data.List          (sortOn)
import           Data.Ord
import           Data.Ratio
import qualified Data.Text          as T

import           QuickSearch.Filter

type Score = Int
type Scorer = (T.Text -> T.Text -> Ratio Int)

data QuickSearch uid = QuickSearch
  { getNames       :: [T.Text]
  -- ^ List of names making up the full search space
  , getuids        :: [uid]
  -- ^ List of UIDs associated with the list of names
  , getTokenFilter :: HMap.HashMap Token (HSet.HashSet uid)
  -- ^ HashMap associating lists of UIDs with individual word-level tokens
  }

-- | Given a string to search, a QuickSearch object, and a similarity function,
-- returns potential matches contained in the QuickSearch filters and their
-- associated scores, in descending order by score.
scoreMatches
  :: (Hashable uid, Eq uid)
  => T.Text -- ^ Name or other string to be searched
  -> QuickSearch uid -- ^ The QuickSearch object to be used
  -> Scorer -- ^ A string distance function of type (Text -> Text -> Ratio Int)
  -> [(Score, (T.Text, uid))] -- ^ A list of possible matches and their scores
scoreMatches (T.toCaseFold -> entry) qs scorer =
  let searchSpace = map (first T.toCaseFold) $ pruneSearchSpace entry qs
      results     = map (toPercent . scorer entry . fst &&& id) searchSpace
  in  sortOn (Down . fst) results

-- | Given a string and a QuickSearch object, return the list of entries
-- from within the QuickSearch that share a full token with the target string
pruneSearchSpace
  :: (Hashable uid, Eq uid) => T.Text -- ^ Name or other string to be searched
  -> QuickSearch uid -- ^ The QuickSearch object to be used
  -> [(T.Text, uid)] -- ^ A list of strings to search through and their UIDs
pruneSearchSpace entry (QuickSearch names uids tokenFilter) =
  let entries      = zip names uids
      uidPartition = getSearchPartition entry tokenFilter
  in  filter ((`HSet.member` uidPartition) . snd) entries

toPercent :: Ratio Int -> Int
toPercent r = floor $ (num / denom) * 100
 where
  ratioToIntPair = fromIntegral . numerator &&& fromIntegral . denominator
  (num, denom)   = ratioToIntPair r
