{-# LANGUAGE BangPatterns #-}

module QuickSearch
  ( buildQuickSearch
  , getMatchesWithCutoff
  , getTopMatches
  , oneShotBatchProcess
  , Token
  , UID
  , Score
  , Scorer
  , QuickSearch(QuickSearch)
  )
where

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

getTopMatches
  :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
getTopMatches n entry quicksearch scorer =
  take n (find entry quicksearch scorer)

getMatchesWithCutoff
  :: Int -> T.Text -> QuickSearch -> Scorer -> [(Score, (T.Text, UID))]
getMatchesWithCutoff cutoff entry quicksearch scorer =
  let results = find entry quicksearch scorer
  in  takeWhile ((> cutoff) . fst) results

oneShotBatchProcess
  :: [(T.Text, UID)]
  -> [(T.Text, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), (Score, (T.Text, UID)))]
oneShotBatchProcess entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> head $ getTopMatches 1 x qs scorer) entries
  in  zip entries results
