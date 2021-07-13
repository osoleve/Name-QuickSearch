{-# LANGUAGE ViewPatterns #-}

module QuickSearch.String (
  buildQuickSearch,
  getTopMatches,
  getMatchesWithCutoff
) where

import           Control.Arrow
import qualified Data.Map         as M
import qualified Data.Text        as T
import           Data.Text.Metrics (damerauLevenshteinNorm, jaro, jaroWinkler)

import           MakeFilter
import           QuickSearch      hiding (buildQuickSearch,
                                   getMatchesWithCutoff, getTopMatches)
import           QuickSearch.Find

buildQuickSearch :: [(String, UID)] -> QuickSearch
buildQuickSearch (map (first T.pack) -> entries) =
  let tokenFilter = buildTokenPartitions entries
  in uncurry QuickSearch (unzip entries) tokenFilter

getTopMatches :: Int -> String -> QuickSearch -> Scorer -> [(Score, (String, UID))]
getTopMatches n (T.pack -> entry) quicksearch scorer =
  let results = take n (find entry quicksearch scorer)
      resultsTextToString = map ((second . first) T.unpack)
  in resultsTextToString results

getMatchesWithCutoff :: Int -> String -> QuickSearch -> Scorer -> [(Score, (String, UID))]
getMatchesWithCutoff cutoff (T.pack -> entry) quicksearch scorer =
  let results = find entry quicksearch scorer
      resultsTextToString = map ((second . first) T.unpack)
  in resultsTextToString . takeWhile ((> cutoff) . fst) $ results
