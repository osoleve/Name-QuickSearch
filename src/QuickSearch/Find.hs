{-# LANGUAGE ViewPatterns #-}

module QuickSearch.Find
  ( scoreMatches
  , Token
  , UID
  , Score
  , Scorer
  , QuickSearch(QuickSearch)
  )
where

import           Control.Arrow
import qualified Data.HashMap.Lazy  as HMap
import qualified Data.HashSet       as HSet
import           Data.List          (sortOn)
import           Data.Ord
import           Data.Ratio
import qualified Data.Text          as T

import           QuickSearch.Filter

type Score = Int
type Scorer = (T.Text -> T.Text -> Ratio Int)

data QuickSearch = QuickSearch
  { getNames       :: [T.Text]
  , getUIDs        :: [UID]
  , getTokenFilter :: HMap.HashMap Token (HSet.HashSet UID)
  }

scoreMatches :: T.Text -> QuickSearch -> Scorer -> [(Score, Record)]
scoreMatches (T.toCaseFold -> entry) qs scorer =
  let searchSpace = map (first T.toCaseFold) $ pruneSearchSpace entry qs
      results = map (toPercent . scorer entry . fst &&& id) searchSpace
  in  sortOn (Down . fst) results

pruneSearchSpace :: T.Text -> QuickSearch -> [Record]
pruneSearchSpace entry (QuickSearch names uids tokenFilter) =
  let entries      = zip names uids
      uidPartition = getSearchPartition entry tokenFilter
  in filter ((`HSet.member` uidPartition) . snd) entries

toPercent :: Ratio Int -> Int
toPercent r = floor $ (num / denom) * 100
 where
  ratioToIntPair = fromIntegral . numerator &&& fromIntegral . denominator
  (num, denom)   = ratioToIntPair r
