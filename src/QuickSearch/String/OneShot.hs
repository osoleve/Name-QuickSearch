module QuickSearch.String.OneShot
  ( oneShot
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

oneShot
  :: (QuickSearch -> Int -> Scorer -> String -> [(Score, Record)])
  -> Int
  -> [Record]
  -> [Record]
  -> Scorer
  -> [(Record, [(Score, Record)])]
oneShot f n entries targets scorer =
  let qs = buildQuickSearch targets
      results = map (f qs n scorer . fst) entries
  in  zip entries results

oneShotTopNMatches
  :: Int
  -> [Record]
  -> [Record]
  -> Scorer
  -> [(Record, [(Score, Record)])]
oneShotTopNMatches = oneShot topNMatches

oneShotMatchesWithCutoff
  :: Int
  -> [Record]
  -> [Record]
  -> Scorer
  -> [(Record, [(Score, Record)])]
oneShotMatchesWithCutoff = oneShot matchesWithCutoff
