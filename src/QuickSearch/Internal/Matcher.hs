{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module QuickSearch.Internal.Matcher
    ( Score
    , Scorer
    , Match(..)
    , QuickSearch(QuickSearch)
    , scoreMatches
    , matchScore
    , matchEntry
    , quickSearchEntries
    , quickSearchTokenFilter
    ) where

import           Control.Arrow                  ( Arrow((&&&)) )
import           Data.Bifunctor                 ( Bifunctor(..) )
import qualified Data.HashMap.Lazy             as HMap
import qualified Data.HashSet                  as HSet
import           Data.Hashable                  ( Hashable )
import           Data.List                      ( sortBy )
import           Data.Ord                       ( Down(Down)
                                                , comparing
                                                )
import           Data.Ratio                     ( Ratio
                                                , denominator
                                                , numerator
                                                )
import qualified Data.Text                     as T

import           QuickSearch.Internal.Filter    ( Entry(..)
                                                , Token
                                                , entryName
                                                , entryUID
                                                , getSearchPartition
                                                )

type Score = Int
type Scorer = (T.Text -> T.Text -> Ratio Int)

-- | Structure associating a Score with an Entry, for holding search results
newtype Match score entry = Match (score, entry)
  deriving newtype (Show, Eq)

instance Bifunctor Match where
    bimap f g (Match (score, entry)) = Match (f score, g entry)

-- | Score accessor for Match
matchScore :: Match Score (Entry name uid) -> Score
matchScore (Match (score, _)) = score

-- | Entry accessor for Match
matchEntry :: Match Score (Entry name uid) -> Entry name uid
matchEntry (Match (_, entry@(Entry (_, _)))) = entry

{- | List of entries to be searched and a HashMap associating tokens with
   HashSets of UIDs related to entries containing the tokens.
-}
newtype QuickSearch uid =
  QuickSearch ([Entry T.Text uid],
                HMap.HashMap Token (HSet.HashSet uid))
  deriving newtype (Show)

-- | [Entry name uid] accessor for QuickSearch
quickSearchEntries :: (Hashable uid, Eq uid) => QuickSearch uid -> [Entry T.Text uid]
quickSearchEntries (QuickSearch (entries, _)) = entries


-- | tokenFilter accessor for QuickSearch
quickSearchTokenFilter
    :: (Hashable uid, Eq uid) => QuickSearch uid -> HMap.HashMap Token (HSet.HashSet uid)
quickSearchTokenFilter (QuickSearch (_, tokenFilter)) = tokenFilter

{- | Given a string to search, a QuickSearch object, and a similarity function,
   returns potential matches contained in the QuickSearch filters and their
   associated scores, in descending order by score.
-}
scoreMatches
    :: (Hashable uid, Eq uid)
    => T.Text  -- ^ Name or other string to be searched
    -> QuickSearch uid  -- ^ The QuickSearch object to be used
    -> Scorer  -- ^ A string distance function of type (Text -> Text -> Ratio Int)
    -> [Match Score (Entry T.Text uid)]  -- ^ A list of possible matches
scoreMatches (T.toCaseFold -> entry) qs scorer =
    let searchSpace = map (first T.toCaseFold) $ pruneSearchSpace entry qs
        scoreEntry  = toPercent . scorer entry . entryName
        results     = map (Match . (scoreEntry &&& id)) searchSpace
    in  sortBy (comparing (Down . matchScore)) results
-- ^ Ignore the linter here, this is a performance thing

{- | Given a string and a QuickSearch object, return the list of entries
   from within the QuickSearch that share a full token with the target string
-}
pruneSearchSpace
    :: (Hashable uid, Eq uid)
    => T.Text  -- ^ Name or other string to be searched
    -> QuickSearch uid  -- ^ The QuickSearch object to be used
    -> [Entry T.Text uid]  -- ^ A list of strings to search through and their UIDs
pruneSearchSpace entry (QuickSearch (entries, tokenFilter)) =
    let uidPartition = getSearchPartition entry tokenFilter
    in  filter ((`HSet.member` uidPartition) . entryUID) entries

toPercent :: Ratio Int -> Int
toPercent r = floor $ (num / denom) * (100 :: Double)
  where
    ratioToIntPair = fromIntegral . numerator &&& fromIntegral . denominator
    (num, denom)   = ratioToIntPair r
