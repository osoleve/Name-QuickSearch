module QuickSearch.OneShot
  ( oneShot
  , oneShotTopNMatches
  , oneShotMatchesWithThreshold
  )
where

import           Data.Function
import           Data.Hashable
import           Data.List
import           Data.Ratio
import qualified Data.Text         as T
import           Data.Text.Metrics

import           QuickSearch

-- | Turn a match retrieval function into a one-shot batch function.
-- Instead of creating a QuickSearch for reuse, this creates it in the
-- background and discards it when done.
oneShot
  :: (Hashable uid, Eq uid)
  => (QuickSearch uid -> Int -> Scorer -> T.Text -> [(Score, (T.Text, uid))])
  -- ^ Match retrieval function to be converted into a one-shot
  -> Int -- ^ The reference number for the match retrieval function.
  -> [(T.Text, uid)] -- ^ List of entries to be processed
  -> [(T.Text, uid)] -- ^ List of entries making up the search space
  -> Scorer -- ^ Similarity function with type (Text -> Text -> Ratio Int)
  -> [((T.Text, uid), [(Score, (T.Text, uid))])]
    -- ^ List of entries and their matches.
oneShot f n entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (f qs n scorer . fst) entries
  in  zip entries results

-- | One-shot version of topNMatches. Builds the QuickSearch in the background
-- and discards it when finished.
oneShotTopNMatches
  :: (Hashable uid, Eq uid)
  => Int -- ^ N: Number of matches to return
  -> [(T.Text, uid)] -- ^ List of entries to be processed
  -> [(T.Text, uid)] -- ^ List of entries making up the search space
  -> Scorer -- ^ Similarity function with type (Text -> Text -> Ratio Int)
  -> [((T.Text, uid), [(Score, (T.Text, uid))])]
  -- ^ List of entries and up to N of the best matches.
oneShotTopNMatches = oneShot topNMatches

-- | One-shot version of matchesWithThreshold. Builds the QuickSearch in
-- the background and discards it when finished.
oneShotMatchesWithThreshold
  :: (Hashable uid, Eq uid)
  => Int -- ^ Score threshold above which to return matches
  -> [(T.Text, uid)] -- ^ List of entries to be processed
  -> [(T.Text, uid)] -- ^ List of entries making up the search space
  -> Scorer -- ^ Similarity function with type (Text -> Text -> Ratio Int)
  -> [((T.Text, uid), [(Score, (T.Text, uid))])]
  -- ^ List of entries and their matches above the score threshold.
oneShotMatchesWithThreshold = oneShot matchesWithThreshold
