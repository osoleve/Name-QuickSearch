module QuickSearch
  ( buildQuickSearch
  , matchesWithThreshold
  , topNMatches
  , batch
  , batchTopNMatches
  , batchMatchesWithThreshold
  , Token
  , Score
  , Scorer
  , Entry(..)
  , Match(..)
  , QuickSearch(..)
  )
where

import           Data.List          hiding (find)
import           Data.Hashable
import           Data.Ratio
import qualified Data.Text          as T
import           Data.Text.Metrics  (damerauLevenshteinNorm, jaro, jaroWinkler)

import           QuickSearch.Filter
import           QuickSearch.Matcher

-- | Given a list of entries to be searched, create a QuickSearch object.
rawBuildQuickSearch
  :: (Hashable uid, Eq uid)
  => [Entry uid] -- ^ List of entries to be searched
  -> QuickSearch uid -- ^ QuickSearch object holding token partitions
rawBuildQuickSearch entries =
  let tokenFilter = buildTokenPartitions entries
  in  QuickSearch entries tokenFilter

-- | Given a list of pairs of (T.Text, uid) to be searched,
-- create a QuickSearch object.
buildQuickSearch
  :: (Hashable uid, Eq uid)
  => [(T.Text, uid)] -- ^ List of entries to be searched
  -> QuickSearch uid -- ^ QuickSearch object holding token partitions
buildQuickSearch entries =
  let entries' = map (uncurry Entry) entries
      tokenFilter = buildTokenPartitions entries'
  in  QuickSearch entries' tokenFilter

-- | Given a QuickSearch object, scorer, and string, return the top N matches.
topNMatches
  :: (Hashable uid, Eq uid)
  => QuickSearch uid -- ^ QuickSearch object holding token partitions
  -> Int -- ^ N: Number of results to return
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> T.Text -- ^ String to be searched
  -> [Match (Entry uid)] -- ^ Top N most similar entries
topNMatches qs n scorer entry = take n (scoreMatches entry qs scorer)

-- | Given a QuickSearch object, scorer, and string, return all matches with a
-- score greater than the given threshold.
matchesWithThreshold
  :: (Hashable uid, Eq uid)
  => QuickSearch uid -- ^ QuickSearch object holding token partitions
  -> Int -- ^ Threshold score above which to return results
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> T.Text -- ^ String to be searched
  -> [Match (Entry uid)] -- ^ Top N most similar entries
matchesWithThreshold qs cutoff scorer entry =
  let results = scoreMatches entry qs scorer
  in  takeWhile ((>= cutoff) . matchScore) results

-- | Turn a match retrieval function into one that works on lists of entries.
batch
  :: (Hashable uid1, Eq uid1, Hashable uid2, Eq uid2)
  => (QuickSearch uid2 -> Int -> Scorer -> T.Text -> [Match (Entry uid2)])
  -- ^ A match retrieval function, such as topNMatches
  -> QuickSearch uid2 -- ^ QuickSearch object holding token partitions
  -> Int -- ^ The reference number for the match retrieval function.
         -- N for topNMatches, threshold for matchesWithThreshold
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> [(T.Text, uid1)] -- ^ List of entries to be processed
  -> [(Entry uid1, [Match (Entry uid2)])]
  -- ^ List of entries and the results returned for each.
batch f qs n scorer entries =
  let entries' = map (uncurry Entry) entries
      results = map (f qs n scorer . entryName) entries'
  in zip entries' results

-- | Version of topNMatches that processes lists of entries instead of strings.
batchTopNMatches
  :: (Hashable uid1, Eq uid1, Hashable uid2, Eq uid2)
  => QuickSearch uid2 -- ^ QuickSearch object holding token partitions
  -> Int -- ^ N: Number of results to return
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> [(T.Text, uid1)] -- ^ List of entries to be processed
  -> [(Entry uid1, [Match (Entry uid2)])]
  -- ^ List of entries and up to the top N matches for each.
batchTopNMatches = batch topNMatches

-- | Version of matchesWithThreshold that processes lists of entries instead of strings.
batchMatchesWithThreshold
  :: (Hashable uid1, Eq uid1, Hashable uid2, Eq uid2)
  => QuickSearch uid2 -- ^ QuickSearch object holding token partitions
  -> Int -- ^ N: Number of results to return
  -> Scorer -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> [(T.Text, uid1)] -- ^ List of entries to be processed
  -> [(Entry uid1, [Match (Entry uid2)])]
  -- ^ List of entries and their matches above the score threshold.
batchMatchesWithThreshold = batch matchesWithThreshold
