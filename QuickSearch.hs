{-# LANGUAGE BangPatterns #-}

module QuickSearch
  ( buildQuickSearch
  , matchesWithCutoff
  , topNMatches
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
