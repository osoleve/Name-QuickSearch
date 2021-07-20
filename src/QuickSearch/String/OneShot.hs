module QuickSearch.String.OneShot
    ( oneShot
    , oneShotTopNMatches
    , oneShotMatchesWithThreshold
    ) where

import           Data.Hashable                  ( Hashable )
import           Data.Text.Metrics              ( )

import           QuickSearch.String             ( Entry(..)
                                                , Match
                                                , QuickSearch
                                                , Score
                                                , Scorer
                                                , buildQuickSearch
                                                , matchesWithThreshold
                                                , topNMatches
                                                )

{- | Turn a match retrieval function into a one-shot batch function.
   Instead of creating a QuickSearch for reuse, this creates it in the
   background and discards it when done.
-}
oneShot
    :: (Hashable uid1, Eq uid1, Hashable uid2, Eq uid2)
    => (  QuickSearch uid2
       -> Int
       -> Scorer
       -> String
       -> [Match Score (Entry String uid2)]
       )
  -- ^ Match retrieval function to be converted into a one-shot
    -> Int  -- ^ The reference number for the match retrieval function.
    -> [(String, uid1)]  -- ^ List of entries to be processed
    -> [(String, uid2)]  -- ^ List of entries making up the search space
    -> Scorer  -- ^ Similarity function with type (Text -> Text -> Ratio Int)
    -> [(Entry String uid1, [Match Score (Entry String uid2)])]
    -- ^ List of entries and their matches.
oneShot f n entries targets scorer =
    let qs      = buildQuickSearch targets
        results = map (f qs n scorer . fst) entries
    in  zip (map Entry entries) results

{- | One-shot version of topNMatches. Builds the QuickSearch in the background
   and discards it when finished.
-}
oneShotTopNMatches
    :: (Hashable uid1, Eq uid1, Hashable uid2, Eq uid2)
    => Int  -- ^ N: Number of matches to return
    -> [(String, uid1)]  -- ^ List of entries to be processed
    -> [(String, uid2)]  -- ^ List of entries making up the search space
    -> Scorer  -- ^ Similarity function with type (Text -> Text -> Ratio Int)
    -> [(Entry String uid1, [Match Score (Entry String uid2)])]
  -- ^ List of entries and up to N of the best matches.
oneShotTopNMatches = oneShot topNMatches

{- | One-shot version of matchesWithThreshold. Builds the QuickSearch in
   the background and discards it when finished.
-}
oneShotMatchesWithThreshold
    :: (Hashable uid1, Eq uid1, Hashable uid2, Eq uid2)
    => Int  -- ^ Score threshold above which to return matches
    -> [(String, uid1)]  -- ^ List of entries to be processed
    -> [(String, uid2)]  -- ^ List of entries making up the search space
    -> Scorer  -- ^ Similarity function with type (Text -> Text -> Ratio Int)
    -> [(Entry String uid1, [Match Score (Entry String uid2)])]
  -- ^ List of entries and their matches above the score threshold.
oneShotMatchesWithThreshold = oneShot matchesWithThreshold
