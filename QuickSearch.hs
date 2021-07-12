module QuickSearch (
  buildQuickSearch,
  getMatchesWithCutoff,
  getTopMatches,
  Token,
  UID,
  Score,
  Scorer,
  QuickSearch(QuickSearch)
) where

import           Control.Arrow
import           Data.List         hiding (find)
import qualified Data.Map          as M
import           Data.Ord
import           Data.Ratio
import qualified Data.Text         as T
import           Data.Text.Metrics

import           MakeFilter
import           QuickSearch.Find

buildQuickSearch :: [(T.Text, UID)] -> QuickSearch
buildQuickSearch entries =
  let tokenFilter = buildTokenPartitions entries
  in uncurry QuickSearch (unzip entries) tokenFilter

getTopMatches :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
getTopMatches n entry quicksearch scorer = take n (find entry quicksearch scorer)

getMatchesWithCutoff :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
getMatchesWithCutoff cutoff entry quicksearch scorer =
  let results = find entry quicksearch scorer
  in takeWhile ((> cutoff) . fst) results
