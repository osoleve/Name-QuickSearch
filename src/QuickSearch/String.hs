{-# LANGUAGE ViewPatterns #-}

module QuickSearch.String
  ( buildQuickSearch
  , topNMatches
  , matchesWithCutoff
  , batchTopNMatches
  , batchMatchesWithCutoff
  , Token
  , Score
  , Scorer
  , QuickSearch(QuickSearch)
  )
where

import           Control.Arrow
import           Data.Hashable
import           Data.Ratio
import qualified Data.Text          as T
import           Data.Text.Metrics  (damerauLevenshteinNorm, jaro, jaroWinkler)

import           QuickSearch        hiding (batchMatchesWithCutoff,
                                     batchTopNMatches, buildQuickSearch,
                                     matchesWithCutoff, oneShotBatchProcess,
                                     topNMatches)
import           QuickSearch.Filter
import           QuickSearch.MatchAndScore

buildQuickSearch :: (Hashable uid, Eq uid) => [(String, uid)] -> QuickSearch uid
buildQuickSearch (map (first T.pack) -> entries) =
  let tokenFilter = buildTokenPartitions entries
  in  uncurry QuickSearch (unzip entries) tokenFilter

topNMatches
  :: (Hashable uid, Eq uid) => QuickSearch uid -> Int -> Scorer -> String -> [(Score, (String, uid))]
topNMatches qs n scorer (T.pack -> entry) =
  let results             = take n (scoreMatches entry qs scorer)
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString results

matchesWithCutoff
  :: (Hashable uid, Eq uid) => QuickSearch uid -> Int -> Scorer -> String -> [(Score, (String, uid))]
matchesWithCutoff qs cutoff scorer (T.pack -> entry) =
  let results             = scoreMatches entry qs scorer
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString . takeWhile ((>= cutoff) . fst) $ results


batch
  :: (Hashable uid, Eq uid) => (QuickSearch uid -> Int -> Scorer -> String -> [(Score, (String, uid))])
  -> QuickSearch uid
  -> Int
  -> Scorer
  -> [(String, uid)]
  -> [((String, uid), [(Score, (String, uid))])]
batch f qs n scorer entries =
  let results = map (f qs n scorer . fst) entries
  in  zip entries results

batchTopNMatches
  :: (Hashable uid, Eq uid) => QuickSearch uid
  -> Int
  -> Scorer
  -> [(String, uid)]
  -> [((String, uid), [(Score, (String, uid))])]
batchTopNMatches = batch topNMatches

batchMatchesWithCutoff
  :: (Hashable uid, Eq uid) => QuickSearch uid
  -> Int
  -> Scorer
  -> [(String, uid)]
  -> [((String, uid), [(Score, (String, uid))])]
batchMatchesWithCutoff = batch matchesWithCutoff
