{-# LANGUAGE ViewPatterns #-}

module QuickSearch.String
  ( buildQuickSearch
  , topNMatches
  , matchesWithCutoff
  , batchTopNMatches
  , batchMatchesWithCutoff
  , Token
  , UID
  , Score
  , Scorer
  , QuickSearch(QuickSearch)
  )
where

import           Control.Arrow
import           Data.Ratio
import qualified Data.Text          as T
import           Data.Text.Metrics  (damerauLevenshteinNorm, jaro, jaroWinkler)

import           QuickSearch        hiding (batchMatchesWithCutoff,
                                     batchTopNMatches, buildQuickSearch,
                                     matchesWithCutoff, oneShotBatchProcess,
                                     topNMatches)
import           QuickSearch.Filter
import           QuickSearch.Find

buildQuickSearch :: [(String, UID)] -> QuickSearch
buildQuickSearch (map (first T.pack) -> entries) =
  let tokenFilter = buildTokenPartitions entries
  in  uncurry QuickSearch (unzip entries) tokenFilter

topNMatches
  :: QuickSearch -> Int -> Scorer -> String -> [(Score, (String, UID))]
topNMatches qs n scorer (T.pack -> entry) =
  let results             = take n (find entry qs scorer)
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString results

matchesWithCutoff
  :: QuickSearch -> Int -> Scorer -> String -> [(Score, (String, UID))]
matchesWithCutoff qs cutoff scorer (T.pack -> entry) =
  let results             = find entry qs scorer
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString . takeWhile ((>= cutoff) . fst) $ results

batchTopNMatches
  :: QuickSearch
  -> Int
  -> (T.Text -> T.Text -> Ratio Int)
  -> [(String, UID)]
  -> [((String, UID), [(Score, (String, UID))])]
batchTopNMatches qs n scorer entries =
  let results = map (topNMatches qs n scorer . fst) entries
  in  zip entries results

batchMatchesWithCutoff
  :: QuickSearch
  -> Int
  -> (T.Text -> T.Text -> Ratio Int)
  -> [(String, UID)]
  -> [((String, UID), [(Score, (String, UID))])]
batchMatchesWithCutoff qs cutoff scorer entries =
  let results = map (matchesWithCutoff qs cutoff scorer . fst) entries
  in  zip entries results
