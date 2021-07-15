{-# LANGUAGE BangPatterns #-}

module QuickSearch
  ( buildQuickSearch
  , matchesWithCutoff
  , topNMatches
  , batchTopNMatches
  , batchMatchesWithCutoff
  , Token
  , UID
  , Score
  , Scorer
  , QuickSearch(QuickSearch)
  )
where

import           Data.Function
import           Data.List          hiding (find)
import qualified Data.Map           as M
import           Data.Ratio
import qualified Data.Text          as T
import           Data.Text.Metrics  (damerauLevenshteinNorm, jaro, jaroWinkler)

import           QuickSearch.Filter
import           QuickSearch.Find

buildQuickSearch :: [(T.Text, UID)] -> QuickSearch
buildQuickSearch entries =
  let !tokenFilter = buildTokenPartitions entries
  in  uncurry QuickSearch (unzip entries) tokenFilter

topNMatches
  :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
topNMatches n entry quicksearch scorer = take n (find entry quicksearch scorer)

matchesWithCutoff
  :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
matchesWithCutoff cutoff entry quicksearch scorer =
  let results = find entry quicksearch scorer
  in  takeWhile ((>= cutoff) . fst) results

batchTopNMatches
  :: Int
  -> [(T.Text, UID)]
  -> QuickSearch
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
batchTopNMatches n entries qs scorer =
  let results = map (\(x, _) -> topNMatches n x qs scorer) entries
  in  zip entries results

batchMatchesWithCutoff
  :: Int
  -> [(T.Text, UID)]
  -> QuickSearch
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
batchMatchesWithCutoff cutoff entries qs scorer =
  let results = map (\(x, _) -> matchesWithCutoff cutoff x qs scorer) entries
  in  zip entries results
