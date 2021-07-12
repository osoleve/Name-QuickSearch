module QuickSearch.Find (
  find,
  Token,
  UID,
  Score,
  Scorer,
  QuickSearch(QuickSearch)
) where

import           Control.Arrow
import           Data.HashSet  as HSet hiding (filter, map)
import           Data.List     (sortOn)
import qualified Data.Map      as M
import           Data.Ord
import           Data.Ratio
import qualified Data.Text     as T

import           MakeFilter

type Token = T.Text
type UID = Int
type Score = Int
type Scorer = (T.Text -> T.Text -> Ratio Int)

data QuickSearch = QuickSearch {
  getNames       :: [T.Text],
  getUIDs        :: [UID],
  getTokenFilter :: M.Map Token (HSet.HashSet UID)
}

find :: T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
find entry (QuickSearch names uids tokenFilter) scorer =
  let entries = zip names uids
      uidPartition = getSearchPartition entry tokenFilter
      searchSpace = filter (flip HSet.member uidPartition . snd) entries
      results = map (\it@(x,_) -> (toPercent $ scorer entry x, it)) searchSpace
  in sortOn (Down . fst) results

toPercent :: Ratio Int -> Int
toPercent r = floor $ (num / denom) * 100
  where
    ratioToIntPair = fromIntegral . numerator &&& fromIntegral . denominator
    (num, denom) = ratioToIntPair r
