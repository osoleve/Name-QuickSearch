module QuickSearch.OneShot
  ( oneShotGetBestMatches
  , oneShotTopNMatches
  , oneShotMatchesWithCutoff
  )
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
      results = map (matchesWithCutoff qs 0 scorer . fst) entries
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
      results = map (topNMatches qs n scorer . fst) entries
  in  zip entries results

oneShotMatchesWithCutoff
  :: Int
  -> [(T.Text, UID)]
  -> [(T.Text, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((T.Text, UID), [(Score, (T.Text, UID))])]
oneShotMatchesWithCutoff cutoff entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (matchesWithCutoff qs cutoff scorer . fst) entries
  in  zip entries results
