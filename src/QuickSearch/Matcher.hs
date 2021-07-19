{-# LANGUAGE ViewPatterns #-}

module QuickSearch.Matcher
  ( scoreMatches
  , Token
  , Score
  , Scorer
  , Match(..)
  , QuickSearch(QuickSearch)
  )
where

import           Control.Arrow      (Arrow ((&&&)))
import qualified Data.HashMap.Lazy  as HMap
import qualified Data.HashSet       as HSet
import           Data.Hashable      (Hashable)
import           Data.List          (sortBy)
import           Data.Ord           (Down (Down), comparing)
import           Data.Ratio         (Ratio, denominator, numerator)
import qualified Data.Text          as T

import           QuickSearch.Filter (Entry (..), Token, first,
                                     getSearchPartition)

type Score = Int
type Scorer = (T.Text -> T.Text -> Ratio Int)

data Match a = Match {
    matchScore :: Score
  , matchEntry :: a
} deriving (Show)

data QuickSearch uid = QuickSearch
  { quickSearchEntries     :: [Entry uid]
  -- ^ List of names and UIDs
  , quickSearchTokenFilter :: HMap.HashMap Token (HSet.HashSet uid)
  -- ^ HashMap associating lists of UIDs with individual word-level tokens
  }

{- | Given a string to search, a QuickSearch object, and a similarity function,
   returns potential matches contained in the QuickSearch filters and their
   associated scores, in descending order by score.
-}
scoreMatches
  :: (Hashable uid, Eq uid)
  => T.Text  -- ^ Name or other string to be searched
  -> QuickSearch uid  -- ^ The QuickSearch object to be used
  -> Scorer  -- ^ A string distance function of type (Text -> Text -> Ratio Int)
  -> [Match (Entry uid)]  -- ^ A list of possible matches and their scores
scoreMatches (T.toCaseFold -> entry) qs scorer =
  let searchSpace = map (first T.toCaseFold) $ pruneSearchSpace entry qs
      scoreEntry = toPercent . scorer entry . entryName
      results     = map (uncurry Match . (scoreEntry &&& id)) searchSpace
  in  sortBy (comparing (Down . matchScore)) results
-- ^ Ignore the linter here, this is a performance thing

{- | Given a string and a QuickSearch object, return the list of entries
   from within the QuickSearch that share a full token with the target string
-}
pruneSearchSpace
  :: (Hashable uid, Eq uid) => T.Text  -- ^ Name or other string to be searched
  -> QuickSearch uid  -- ^ The QuickSearch object to be used
  -> [Entry uid]  -- ^ A list of strings to search through and their UIDs
pruneSearchSpace entry (QuickSearch entries tokenFilter) =
  let uidPartition = getSearchPartition entry tokenFilter
  in  filter ((`HSet.member` uidPartition) . entryUID) entries

toPercent :: Ratio Int -> Int
toPercent r = floor $ (num / denom) * 100
 where
  ratioToIntPair = fromIntegral . numerator &&& fromIntegral . denominator
  (num, denom)   = ratioToIntPair r
