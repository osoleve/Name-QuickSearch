module QuickSearch.OneShot
  (oneShotGetBestMatches, oneShotTopNMatches, oneShotMatchesWithCutoff)
  where

import           Data.Function
import           Data.List
import           Data.Ratio
import qualified Data.Text         as T
import           Data.Text.Metrics

import           QuickSearch

oneShotGetBestMatches
  :: [(T.Text, UID)]
  -> [(T.Text, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
oneShotGetBestMatches entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> matchesWithCutoff 0 x qs scorer) entries
      bests   = map (head . groupBy ((==) `on` fst)) results
  in  zip entries bests

oneShotTopNMatches
  :: Int
  -> [(T.Text, UID)]
  -> [(T.Text, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
oneShotTopNMatches n entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> topNMatches n x qs scorer) entries
  in  zip entries results

oneShotMatchesWithCutoff
  :: Int
  -> [(T.Text, UID)]
  -> [(T.Text, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
oneShotMatchesWithCutoff cutoff entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> matchesWithCutoff cutoff x qs scorer) entries
  in  zip entries results
