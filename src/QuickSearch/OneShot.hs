module QuickSearch.OneShot
  ( oneShot
  , oneShotTopNMatches
  , oneShotMatchesWithCutoff
  )
where

import           Data.Function
import           Data.Hashable
import           Data.List
import           Data.Ratio
import qualified Data.Text         as T
import           Data.Text.Metrics

import           QuickSearch

oneShot
  :: (Hashable uid, Eq uid) => (QuickSearch uid -> Int -> Scorer -> T.Text -> [(Score, (T.Text, uid))])
  -> Int
  -> [(T.Text, uid)]
  -> [(T.Text, uid)]
  -> Scorer
  -> [((T.Text, uid), [(Score, (T.Text, uid))])]
oneShot f n entries targets scorer =
  let qs = buildQuickSearch targets
      results = map (f qs n scorer . fst) entries
  in  zip entries results

oneShotTopNMatches
  :: (Hashable uid, Eq uid) => Int
  -> [(T.Text, uid)]
  -> [(T.Text, uid)]
  -> Scorer
  -> [((T.Text, uid), [(Score, (T.Text, uid))])]
oneShotTopNMatches = oneShot topNMatches

oneShotMatchesWithCutoff
  :: (Hashable uid, Eq uid) => Int
  -> [(T.Text, uid)]
  -> [(T.Text, uid)]
  -> Scorer
  -> [((T.Text, uid), [(Score, (T.Text, uid))])]
oneShotMatchesWithCutoff = oneShot matchesWithCutoff
