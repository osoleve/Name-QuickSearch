module QuickSearch.String.OneShot
  ( oneShot
  , oneShotTopNMatches
  , oneShotMatchesWithCutoff
  )
where

import           Data.Function
import           Data.Hashable
import           Data.List
import           Data.Ratio
import qualified Data.Text          as T
import           Data.Text.Metrics

import           QuickSearch.String

oneShot
  :: (Hashable uid, Eq uid) => (QuickSearch uid -> Int -> Scorer -> String -> [(Score, (String, uid))])
  -> Int
  -> [(String, uid)]
  -> [(String, uid)]
  -> Scorer
  -> [((String, uid), [(Score, (String, uid))])]
oneShot f n entries targets scorer =
  let qs = buildQuickSearch targets
      results = map (f qs n scorer . fst) entries
  in  zip entries results

oneShotTopNMatches
  :: (Hashable uid, Eq uid) => Int
  -> [(String, uid)]
  -> [(String, uid)]
  -> Scorer
  -> [((String, uid), [(Score, (String, uid))])]
oneShotTopNMatches = oneShot topNMatches

oneShotMatchesWithCutoff
  :: (Hashable uid, Eq uid) => Int
  -> [(String, uid)]
  -> [(String, uid)]
  -> Scorer
  -> [((String, uid), [(Score, (String, uid))])]
oneShotMatchesWithCutoff = oneShot matchesWithCutoff
