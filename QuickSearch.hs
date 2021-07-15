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
  :: QuickSearch -> Int -> Scorer -> T.Text -> [(Score, (T.Text, UID))]
topNMatches qs n scorer entry = take n (find entry qs scorer)

matchesWithCutoff
  :: QuickSearch -> Int -> Scorer -> T.Text -> [(Score, (T.Text, UID))]
matchesWithCutoff qs cutoff scorer entry =
  let results = find entry qs scorer
  in  takeWhile ((>= cutoff) . fst) results

batchTopNMatches
  :: QuickSearch
  -> Int
  -> (T.Text -> T.Text -> Ratio Int)
  -> [(T.Text, UID)]
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
batchTopNMatches qs n scorer entries =
  let results = map (topNMatches qs n scorer . fst) entries
  in  zip entries results

batchMatchesWithCutoff
  :: QuickSearch
  -> Int
  -> (T.Text -> T.Text -> Ratio Int)
  -> [(T.Text, UID)]
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
batchMatchesWithCutoff qs cutoff scorer entries =
  let results = map (matchesWithCutoff qs cutoff scorer . fst) entries
  in  zip entries results
