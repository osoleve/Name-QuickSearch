{-# LANGUAGE ViewPatterns #-}

module QuickSearch.String
  ( buildQuickSearch
  , topNMatches
  , matchesWithCutoff
  , batchTopNMatches
  , batchMatchesWithCutoff
  , Token
  , UID
  , Record
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
                                     topNMatches, Record)
import           QuickSearch.Filter hiding (Record)
import           QuickSearch.Find

type Record = (String, UID)

buildQuickSearch :: [Record] -> QuickSearch
buildQuickSearch (map (first T.pack) -> entries) =
  let tokenFilter = buildTokenPartitions entries
  in  uncurry QuickSearch (unzip entries) tokenFilter

topNMatches
  :: QuickSearch -> Int -> Scorer -> String -> [(Score, Record)]
topNMatches qs n scorer (T.pack -> entry) =
  let results             = take n (scoreMatches entry qs scorer)
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString results

matchesWithCutoff
  :: QuickSearch -> Int -> Scorer -> String -> [(Score, Record)]
matchesWithCutoff qs cutoff scorer (T.pack -> entry) =
  let results             = scoreMatches entry qs scorer
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString . takeWhile ((>= cutoff) . fst) $ results


batch
  :: (QuickSearch -> Int -> Scorer -> String -> [(Score, Record)])
  -> QuickSearch
  -> Int
  -> Scorer
  -> [Record]
  -> [(Record, [(Score, Record)])]
batch f qs n scorer entries =
  let results = map (f qs n scorer . fst) entries
  in  zip entries results

batchTopNMatches
  :: QuickSearch
  -> Int
  -> Scorer
  -> [Record]
  -> [(Record, [(Score, Record)])]
batchTopNMatches = batch topNMatches

batchMatchesWithCutoff
  :: QuickSearch
  -> Int
  -> Scorer
  -> [Record]
  -> [(Record, [(Score, Record)])]
batchMatchesWithCutoff = batch matchesWithCutoff
