module QuickSearch.String (
  buildQuickSearch,
  getTopMatches,
  getMatchesWithCutoff
) where

import           Control.Arrow
import qualified Data.Text  as T
import           Data.Ratio
import qualified Data.Map   as M
import           Data.Text.Metrics

import MakeFilter
import QuickSearch
  hiding (buildQuickSearch, getTopMatches, getMatchesWithCutoff)
import QuickSearch.Find

buildQuickSearch :: [(String, UID)] -> QuickSearch
buildQuickSearch entries =
  let entries' = map (first T.pack) entries
      tokenFilter = buildTokenPartitions entries'
  in uncurry QuickSearch (unzip entries') tokenFilter

getTopMatches :: Int -> String -> QuickSearch -> Scorer -> [(Score, (String, UID))]
getTopMatches n entry quicksearch scorer =
  let results = take n (find (T.pack entry) quicksearch scorer)
  in map ((second . first) T.unpack) results

getMatchesWithCutoff :: Int -> String -> QuickSearch -> Scorer -> [(Score, (String, UID))]
getMatchesWithCutoff cutoff entry quicksearch scorer =
  let results = takeWhile ((> cutoff) . fst) $ find (T.pack entry) quicksearch scorer
  in map ((second . first) T.unpack) results
