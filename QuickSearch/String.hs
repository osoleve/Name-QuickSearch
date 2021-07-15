{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module QuickSearch.String
  ( buildQuickSearch
  , topNMatches
  , matchesWithCutoff
  , Token
  , UID
  , Score
  , Scorer
  , QuickSearch(QuickSearch)
  )
where

import           Control.Arrow
import           Data.Function
import qualified Data.Map           as M
import           Data.Ratio
import qualified Data.Text          as T
import           Data.Text.Metrics  (damerauLevenshteinNorm, jaro, jaroWinkler)

import           QuickSearch        hiding (buildQuickSearch,
                                     matchesWithCutoff, topNMatches,
                                     oneShotBatchProcess)
import           QuickSearch.Filter
import           QuickSearch.Find

buildQuickSearch :: [(String, UID)] -> QuickSearch
buildQuickSearch (map (first T.pack) -> entries) =
  let !tokenFilter = buildTokenPartitions entries
  in  uncurry QuickSearch (unzip entries) tokenFilter

topNMatches
  :: Int -> String -> QuickSearch -> Scorer -> [(Score, (String, UID))]
topNMatches n (T.pack -> entry) quicksearch scorer =
  let results             = take n (find entry quicksearch scorer)
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString results

matchesWithCutoff
  :: Int -> String -> QuickSearch -> Scorer -> [(Score, (String, UID))]
matchesWithCutoff cutoff (T.pack -> entry) quicksearch scorer =
  let results             = find entry quicksearch scorer
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString . takeWhile ((>= cutoff) . fst) $ results
