{-# LANGUAGE BangPatterns #-}

module QuickSearch
  ( buildQuickSearch
  , matchesWithCutoff
  , topNMatches
  , batchGetBestMatches
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
import           Data.List hiding (find)
import qualified Data.Map                      as M
import           Data.Ratio
import qualified Data.Text                     as T
import           Data.Text.Metrics              ( damerauLevenshteinNorm
                                                , jaro
                                                , jaroWinkler
                                                )

import           QuickSearch.Filter
import           QuickSearch.Find

buildQuickSearch :: [(T.Text, UID)] -> QuickSearch
buildQuickSearch entries =
  let !tokenFilter = buildTokenPartitions entries
  in  uncurry QuickSearch (unzip entries) tokenFilter

topNMatches
  :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
topNMatches n entry quicksearch scorer =
  take n (find entry quicksearch scorer)

matchesWithCutoff
  :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
matchesWithCutoff cutoff entry quicksearch scorer =
  let results = find entry quicksearch scorer
  in  takeWhile ((>= cutoff) . fst) results

batchGetBestMatches
  :: [(T.Text, UID)]
  -> [(T.Text, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
batchGetBestMatches entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> matchesWithCutoff 0 x qs scorer) entries
      bests   = map (head . groupBy ((==) `on` fst)) results
  in  zip entries bests

batchTopNMatches
  :: Int
  -> [(T.Text, UID)]
  -> [(T.Text, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
batchTopNMatches n entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> topNMatches n x qs scorer) entries
  in  zip entries results

batchMatchesWithCutoff
  :: Int
  -> [(T.Text, UID)]
  -> [(T.Text, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
batchMatchesWithCutoff cutoff entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> matchesWithCutoff cutoff x qs scorer) entries
  in  zip entries results
