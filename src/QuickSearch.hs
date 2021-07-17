module QuickSearch
  ( buildQuickSearch
  , matchesWithCutoff
  , topNMatches
  , batchTopNMatches
  , batchMatchesWithCutoff
  , Token
  , Score
  , Scorer
  , QuickSearch(QuickSearch)
  )
where

import           Data.List          hiding (find)
import           Data.Hashable
import           Data.Ratio
import qualified Data.Text          as T
import           Data.Text.Metrics  (damerauLevenshteinNorm, jaro, jaroWinkler)

import           QuickSearch.Filter
import           QuickSearch.MatchAndScore

buildQuickSearch :: (Hashable uid, Eq uid) => [(T.Text, uid)] -> QuickSearch uid
buildQuickSearch entries =
  let tokenFilter = buildTokenPartitions entries
  in  uncurry QuickSearch (unzip entries) tokenFilter

topNMatches
  :: (Hashable uid, Eq uid) => QuickSearch uid -> Int -> Scorer -> T.Text -> [(Score, (T.Text, uid))]
topNMatches qs n scorer entry = take n (scoreMatches entry qs scorer)

matchesWithCutoff
  :: (Hashable uid, Eq uid) => QuickSearch uid -> Int -> Scorer -> T.Text -> [(Score, (T.Text, uid))]
matchesWithCutoff qs cutoff scorer entry =
  let results = scoreMatches entry qs scorer
  in  takeWhile ((>= cutoff) . fst) results

batch
  :: (Hashable uid, Eq uid) => (QuickSearch uid -> Int -> Scorer -> T.Text -> [(Score, (T.Text, uid))])
  -> QuickSearch uid
  -> Int
  -> Scorer
  -> [(T.Text, uid)]
  -> [((T.Text, uid), [(Score, (T.Text, uid))])]
batch f qs n scorer entries =
  let results = map (f qs n scorer . fst) entries
  in  zip entries results

batchTopNMatches
  :: (Hashable uid, Eq uid) => QuickSearch uid
  -> Int
  -> Scorer
  -> [(T.Text, uid)]
  -> [((T.Text, uid), [(Score, (T.Text, uid))])]
batchTopNMatches = batch topNMatches

batchMatchesWithCutoff
  :: (Hashable uid, Eq uid) => QuickSearch uid
  -> Int
  -> (T.Text -> T.Text -> Ratio Int)
  -> [(T.Text, uid)]
  -> [((T.Text, uid), [(Score, (T.Text, uid))])]
batchMatchesWithCutoff = batch matchesWithCutoff
