module QuickSearch (buildQuickSearch, getMatchesWithCutoff, getTopMatches) where

import           Control.Arrow
import qualified Data.Bifunctor    as B
import           Data.List         hiding (find)
import qualified Data.Map          as M
import           Data.Ord
import           Data.Ratio
import qualified Data.Text         as T
import           Data.Text.Metrics

import           MakeFilter

type Token = T.Text
type UID = Int
type Score = Int
type Scorer = (T.Text -> T.Text -> Ratio Int)

data QuickSearch = QuickSearch {
  getNames       :: [T.Text],
  getUIDs        :: [UID],
  getTokenFilter :: M.Map Token [UID]
}

buildQuickSearch :: [(String, UID)] -> QuickSearch
buildQuickSearch entries =
  let entries' = map (B.first T.pack) entries
      tokenFilter = buildTokenPartitions entries'
  in QuickSearch (map fst entries') (map snd entries') tokenFilter

toPercent :: Ratio Int -> Int
toPercent r = floor $ (num / denom) * 100
  where
    ratioToIntPair = fromIntegral . numerator &&& fromIntegral . denominator
    (num, denom) = ratioToIntPair r

find :: T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
find entry (QuickSearch names uids tokenFilter) scorer =
  let entries = zip names uids
      uidPartition = getSearchPartition entry tokenFilter
      searchSpace = filter ((`elem` uidPartition) . snd) entries
      results = map (\it@(x,_) -> (toPercent $ scorer entry x, it)) searchSpace
  in sortOn (Down . fst) results

getTopMatches :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
getTopMatches n entry quicksearch scorer = take n (find entry quicksearch scorer)

getMatchesWithCutoff :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
getMatchesWithCutoff cutoff entry quicksearch scorer =
  let results = find entry quicksearch scorer
  in takeWhile ((> cutoff) . fst) results
