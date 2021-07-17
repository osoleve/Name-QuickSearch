{-# LANGUAGE ViewPatterns #-}

module QuickSearch.String
  ( buildQuickSearch
  , topNMatches
  , matchesWithThreshold
  , batchTopNMatches
  , batchMatchesWithThreshold
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

import           QuickSearch        hiding (batchMatchesWithThreshold,
                                     batchTopNMatches, buildQuickSearch,
                                     matchesWithThreshold, topNMatches)
import           QuickSearch.Filter
import           QuickSearch.MatchAndScore

-- | Given a list of entries to be searched, create a QuickSearch object.
buildQuickSearch
  :: (Hashable uid, Eq uid)
  => [(String, uid)] -- ^ List of entries to be searched
  -> QuickSearch uid -- ^ QuickSearch object holding token partitions
buildQuickSearch (map (first T.pack) -> entries) =
  let tokenFilter = buildTokenPartitions entries
  in  uncurry QuickSearch (unzip entries) tokenFilter

-- | Given a QuickSearch object, scorer, and string, return the top N matches.
topNMatches
  :: (Hashable uid, Eq uid)
  => QuickSearch uid -- ^ QuickSearch object holding token partitions
  -> Int -- ^ N: Number of results to return
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> String -- ^ String to be searched
  -> [(Score, (String, uid))] -- ^ Top N most similar entries
topNMatches qs n scorer (T.pack -> entry) =
  let results             = take n (scoreMatches entry qs scorer)
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString results
  
-- | Given a QuickSearch object, scorer, and string, return all matches with a
-- score greater than the given threshold.
matchesWithThreshold
  :: (Hashable uid, Eq uid)
  => QuickSearch uid -- ^ QuickSearch object holding token partitions
  -> Int -- ^ Threshold score above which to return results
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> String -- ^ String to be searched
  -> [(Score, (String, uid))] -- ^ Top N most similar entries
matchesWithThreshold qs cutoff scorer (T.pack -> entry) =
  let results             = scoreMatches entry qs scorer
      resultsTextToString = map ((second . first) T.unpack)
  in  resultsTextToString . takeWhile ((>= cutoff) . fst) $ results

-- | Turn a match retrieval function into one that works on lists of entries.
batch
  :: (Hashable uid, Eq uid)
  => (QuickSearch uid -> Int -> Scorer -> String -> [(Score, (String, uid))])
  -- ^ A match retrieval function, such as topNMatches
  -> QuickSearch uid -- ^ QuickSearch object holding token partitions
  -> Int -- ^ The reference number for the match retrieval function.
         -- N for topNMatches, threshold for matchesWithThreshold
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> [(String, uid)] -- ^ List of entries to be processed
  -> [((String, uid), [(Score, (String, uid))])]
  -- ^ List of entries and the results returned for each.
batch f qs n scorer entries =
  let results = map (f qs n scorer . fst) entries in zip entries results

-- | Version of topNMatches that processes lists of entries instead of strings.
batchTopNMatches
  :: (Hashable uid, Eq uid)
  => QuickSearch uid -- ^ QuickSearch object holding token partitions
  -> Int -- ^ N: Number of results to return
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> [(String, uid)] -- ^ List of entries to be processed
  -> [((String, uid), [(Score, (String, uid))])]
  -- ^ List of entries and up to the top N matches for each.
batchTopNMatches = batch topNMatches

-- | Version of matchesWithThreshold that processes lists of entries instead of strings.
batchMatchesWithThreshold
  :: (Hashable uid, Eq uid)
  => QuickSearch uid -- ^ QuickSearch object holding token partitions
  -> Int -- ^ N: Number of results to return
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> [(String, uid)] -- ^ List of entries to be processed
  -> [((String, uid), [(Score, (String, uid))])]
  -- ^ List of entries and their matches above the score threshold.
batchMatchesWithThreshold = batch matchesWithThreshold
