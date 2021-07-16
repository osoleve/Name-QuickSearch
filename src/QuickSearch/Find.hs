{-# LANGUAGE ViewPatterns #-}

module QuickSearch.Find
  ( find
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

find :: T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
find (T.toCaseFold -> entry) (QuickSearch names uids tokenFilter) scorer =
  let entries      = zip names uids
      uidPartition = getSearchPartition entry tokenFilter
      searchSpace  = filter ((`HSet.member` uidPartition) . snd) entries
      results =
        map (toPercent . scorer entry . T.toCaseFold . fst &&& id) searchSpace
  in  sortOn (Down . fst) results

toPercent :: Ratio Int -> Int
toPercent r = floor $ (num / denom) * 100
 where
  ratioToIntPair = fromIntegral . numerator &&& fromIntegral . denominator
  (num, denom)   = ratioToIntPair r
