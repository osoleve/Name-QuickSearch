{-# LANGUAGE ViewPatterns #-}

module QuickSearch.String
  ( buildQuickSearch
  , topNMatches
  , matchesWithThreshold
  , batch
  , batchTopNMatches
  , batchMatchesWithThreshold
  , Token
  , Entry(..)
  , Score
  , Scorer
  , Match
  , QuickSearch(..)
  )
where

import           Control.Arrow                (Arrow ((&&&)))
import           Data.Bifunctor               (Bifunctor (first))
import           Data.Hashable                (Hashable)
import qualified Data.Text                    as T
import           Data.Text.Metrics            (damerauLevenshteinNorm, jaro,
                                               jaroWinkler)

import           QuickSearch.Internal.Filter  (Entry (..), Token,
                                               buildTokenPartitions, entryName,
                                               entryUID)
import           QuickSearch.Internal.Matcher (Match (..), QuickSearch (..),
                                               Score, Scorer, matchEntry,
                                               matchScore, scoreMatches)

-- | Convert (Entry String uid) to (Entry T.Text uid)
stringEntryToTextEntry
  :: (Hashable uid, Eq uid)
  => Entry String uid
  -> Entry T.Text uid
stringEntryToTextEntry = first T.pack

-- | Convert (Entry T.Text uid) to (Entry String uid)
textEntryToStringEntry
  :: (Hashable uid, Eq uid) => Entry T.Text uid -> Entry String uid
textEntryToStringEntry = Entry . (T.unpack . entryName &&& entryUID)


-- | Given a Match (Entry String uid), return it as a Match (Entry uid)
scoredStringToText
  :: (Hashable uid, Eq uid)
  => Match Score (Entry String uid)
  -> Match Score (Entry T.Text uid)
scoredStringToText scoredEntry = Match (score, textEntry)
  where
    (score, entry) = (matchScore &&& matchEntry) scoredEntry
    textEntry = stringEntryToTextEntry entry

-- | Given a Match (Entry String uid), return it as a Match (Entry uid)
scoredTextToString
  :: (Hashable uid, Eq uid)
  => Match Score (Entry T.Text uid)
  -> Match Score (Entry String uid)
scoredTextToString scoredEntry = Match (score, textEntry)
  where
    (score, entry) = (matchScore &&& matchEntry) scoredEntry
    textEntry = textEntryToStringEntry entry

-- | Given a list of entries to be searched, create a QuickSearch object.
rawBuildQuickSearch
  :: (Hashable uid, Eq uid)
  => [Entry String uid]  -- ^ List of entries to be searched
  -> QuickSearch uid  -- ^ QuickSearch object holding token partitions
rawBuildQuickSearch entries =
  let entries' = map stringEntryToTextEntry entries
      tokenFilter = buildTokenPartitions entries'
  in  QuickSearch (entries', tokenFilter)

{- | Given a list of pairs of (String, uid) to be searched,
   create a QuickSearch object.
-}
buildQuickSearch
  :: (Hashable uid, Eq uid)
  => [(String, uid)]  -- ^ List of entries to be searched
  -> QuickSearch uid  -- ^ QuickSearch object holding token partitions
buildQuickSearch entries = rawBuildQuickSearch entries'
  where entries' = map Entry entries

-- | Given a QuickSearch object, scorer, and string, return the top N matches.
topNMatches
  :: (Hashable uid, Eq uid)
  => QuickSearch uid  -- ^ QuickSearch object holding token partitions
  -> Int  -- ^ N: Number of results to return
  -> Scorer  -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> String  -- ^ String to be searched
  -> [Match Score (Entry String uid)]  -- ^ Top N most similar entries
topNMatches qs n scorer (T.pack -> entry) = map scoredTextToString results
  where results = take n (scoreMatches entry qs scorer)

{- | Given a QuickSearch object, scorer, and string, return all matches with a
   score greater than the given threshold.
-}
matchesWithThreshold
  :: (Hashable uid, Eq uid)
  => QuickSearch uid  -- ^ QuickSearch object holding token partitions
  -> Int  -- ^ Threshold score above which to return results
  -> Scorer  -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> String  -- ^ String to be searched
  -> [Match Score (Entry String uid)]  -- ^ Top N most similar entries
matchesWithThreshold qs cutoff scorer (T.pack -> entry) =
  let results = scoreMatches entry qs scorer
  in map scoredTextToString results

-- | Turn a match retrieval function into one that works on lists of entries.
batch
  :: (Hashable uid1, Eq uid1, Hashable uid2, Eq uid2)
  => (  QuickSearch uid2
     -> Int
     -> Scorer
     -> String
     -> [Match Score (Entry String uid2)]
     )
  -- ^ A match retrieval function, such as topNMatches
  -> QuickSearch uid2  -- ^ QuickSearch object holding token partitions
  -> Int  {- ^ The reference number for the match retrieval function.
             N for topNMatches, threshold for matchesWithThreshold
          -}
  -> Scorer  -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> [(String, uid1)]  -- ^ List of entries to be processed
  -> [(Entry String uid1, [Match Score (Entry String uid2)])]
  -- ^ List of entries and the results returned for each.
batch f qs n scorer entries =
  let entries' = map Entry entries
      results  = map (f qs n scorer . entryName) entries'
  in  zip entries' results


-- | Version of topNMatches that processes lists of entries instead of strings.
batchTopNMatches
  :: (Hashable uid1, Eq uid1, Hashable uid2, Eq uid2)
  => QuickSearch uid2  -- ^ QuickSearch object holding token partitions
  -> Int  -- ^ N: Number of results to return
  -> Scorer  -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> [(String, uid1)]  -- ^ List of entries to be processed
  -> [(Entry String uid1, [Match Score (Entry String uid2)])]
  -- ^ List of entries and up to the top N matches for each.
batchTopNMatches = batch topNMatches

{- | Version of matchesWithThreshold that processes lists of entries
   instead of strings.
-}
batchMatchesWithThreshold
  :: (Hashable uid1, Eq uid1, Hashable uid2, Eq uid2)
  => QuickSearch uid2  -- ^ QuickSearch object holding token partitions
  -> Int  -- ^ N: Number of results to return
  -> Scorer  -- ^ String similarity function of type (Text -> Text -> Ratio Int)
  -> [(String, uid1)]  -- ^ List of entries to be processed
  -> [(Entry String uid1, [Match Score (Entry String uid2)])]
  -- ^ List of entries and their matches above the score threshold.
batchMatchesWithThreshold = batch matchesWithThreshold
