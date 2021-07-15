module QuickSearch.String.OneShot
  ( oneShotGetBestMatches
  , oneShotTopNMatches
  , oneShotMatchesWithCutoff
  )
where

import           Data.Function
import           Data.List
import           Data.Ratio
import qualified Data.Text          as T
import           Data.Text.Metrics

import           QuickSearch.String

oneShotGetBestMatches
  :: [(String, UID)]
  -> [(String, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((String, UID), [(Score, (String, UID))])]
oneShotGetBestMatches entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> matchesWithCutoff 0 x qs scorer) entries
      bests   = map (head . groupBy ((==) `on` fst)) results
  in  zip entries bests

oneShotTopNMatches
  :: Int
  -> [(String, UID)]
  -> [(String, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((String, UID), [(Score, (String, UID))])]
oneShotTopNMatches n entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> topNMatches n x qs scorer) entries
  in  zip entries results

oneShotMatchesWithCutoff
  :: Int
  -> [(String, UID)]
  -> [(String, UID)]
  -> (T.Text -> T.Text -> Ratio Int)
  -> [((String, UID), [(Score, (String, UID))])]
oneShotMatchesWithCutoff cutoff entries targets scorer =
  let qs      = buildQuickSearch targets
      results = map (\(x, _) -> matchesWithCutoff cutoff x qs scorer) entries
  in  zip entries results
