module QuickSearch
  ( buildQuickSearch
  , matchesWithCutoff
  , topNMatches
  , batchTopNMatches
  , batchMatchesWithCutoff
  , Token
  , UID
  , Record
  , Score
  , Scorer
  , QuickSearch(QuickSearch)
  )
where

import           Data.List          hiding (find)
import           Data.Ratio
import qualified Data.Text          as T
import           Data.Text.Metrics  (damerauLevenshteinNorm, jaro, jaroWinkler)

import           QuickSearch.Filter
import           QuickSearch.Find

buildQuickSearch :: [Record] -> QuickSearch
buildQuickSearch entries =
  let tokenFilter = buildTokenPartitions entries
  in  uncurry QuickSearch (unzip entries) tokenFilter

topNMatches
  :: QuickSearch -> Int -> Scorer -> T.Text -> [(Score, Record)]
topNMatches qs n scorer entry = take n (find entry qs scorer)

matchesWithCutoff
  :: QuickSearch -> Int -> Scorer -> T.Text -> [(Score, Record)]
matchesWithCutoff qs cutoff scorer entry =
  let results = find entry qs scorer
  in  takeWhile ((>= cutoff) . fst) results

batch
  :: (QuickSearch -> Int -> Scorer -> T.Text -> [(Score, Record)])
  -> QuickSearch
  -> Int
  -> Scorer
  -> [Record]
  -> [(Record, [(Score, Record)])]
batch f qs n scorer entries =
  let results = map (f qs n scorer . fst) entries
  in  zip entries results

batchTopNMatches
  :: QuickSearch
  -> Int
  -> Scorer
  -> [Record]
  -> [(Record, [(Score, Record)])]
batchTopNMatches = batch topNMatches

batchMatchesWithCutoff
  :: QuickSearch
  -> Int
  -> (T.Text -> T.Text -> Ratio Int)
  -> [Record]
  -> [(Record, [(Score, Record)])]
batchMatchesWithCutoff = batch matchesWithCutoff
