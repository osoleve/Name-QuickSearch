{-# LANGUAGE ScopedTypeVariables #-}

module QuickSearch.Filter
  ( buildTokenPartitions
  , getSearchPartition
  , Token
  )
where

import           Control.Arrow
import           Data.Char
import           Data.Hashable
import qualified Data.HashMap.Lazy as HMap
import qualified Data.HashSet      as HSet
import           Data.List
import           Data.Maybe
import qualified Data.Text         as T

type Token = T.Text

-- | Turn a Data.Text.Text string into a list of casefolded tokens
getTokens
  :: T.Text -- ^ The target string
  -> [Token] -- ^ A list of tokens from the target string, casefolded
getTokens = T.words . clean . T.toCaseFold
 where
  toDelete = ".'"
  clean :: T.Text -> T.Text
  clean = T.filter (`notElem` toDelete) . T.map cleanChar
  cleanChar :: Char -> Char
  cleanChar c | any ($ c) [isLower, isDigit, isSpace, (`elem` toDelete)] = c
              | otherwise = ' '

-- | Given the list of entries to be held by QuickSearch, return a HashMap
-- keyed on tokens from the strings in the entries, where the associated
-- HashMap value is the list of uids of entries containing the token.
buildTokenPartitions
  :: (Hashable uid, Eq uid)
  => [(T.Text, uid)] -- ^ List of entries
  -> HMap.HashMap Token (HSet.HashSet uid) -- ^ A map of Token -> [uids]
buildTokenPartitions = tokenPartitions . map (first getTokens)

-- | Given a list of tokenized entries to be held by QuickSearch,
-- return a HashMap keyed on the set of distinct tokens where the associated
-- HashMap value is the list of uids of entries containing the token.
tokenPartitions
  :: forall uid . (Hashable uid, Eq uid)
  => [([Token], uid)] -- ^ List of tokenized entries
  -> HMap.HashMap Token (HSet.HashSet uid) -- ^ A map of Token -> [uids]
tokenPartitions entries = HMap.fromList $ map (id &&& allWith) allTokens
 where
  allTokens = nub . concatMap fst $ entries
  allWith :: (Hashable uid) => Token -> HSet.HashSet uid
  allWith token =
    HSet.fromList . map snd $ filter ((token `elem`) . fst) entries

-- | Given a target string and a Token HashMap, return the union of
-- sets of uids associated with the tokens in the target string
getSearchPartition
  :: (Hashable uid, Eq uid)
  => T.Text -- ^ Target string
  -> HMap.HashMap Token (HSet.HashSet uid) -- ^ HashMap associating tokens
                                           -- with sets of uids
  -> HSet.HashSet uid -- ^ The union of sets of associated uids.
getSearchPartition name tokenMap =
  let tokens = getTokens name
  in  HSet.unions $ map (fromMaybe HSet.empty . (`HMap.lookup` tokenMap)) tokens
