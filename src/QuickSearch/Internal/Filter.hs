{-# LANGUAGE ScopedTypeVariables #-}

module QuickSearch.Internal.Filter
  ( buildTokenPartitions
  , getSearchPartition
  , Token
  , Entry(..)
  , first
  )
where

import           Control.Arrow     (Arrow ((&&&)))
import           Data.Char         (isDigit, isLower, isSpace)
import qualified Data.HashMap.Lazy as HMap
import qualified Data.HashSet      as HSet
import           Data.Hashable     (Hashable)
import           Data.Maybe        (fromMaybe)
import qualified Data.Text         as T

type Token = T.Text

data Entry uid = Entry {
    entryName :: T.Text
  , entryUID  :: uid
} deriving (Show)

first :: (T.Text -> T.Text) -> Entry uid -> Entry uid
first f entry = Entry (f . entryName $ entry) (entryUID entry)

toTokenizedTuple :: (Hashable uid, Eq uid) => Entry uid -> ([Token], uid)
toTokenizedTuple = getTokens . entryName &&& entryUID

-- | Turn a Data.Text.Text string into a list of casefolded tokens
getTokens
  :: T.Text  -- ^ The target string
  -> [Token]  -- ^ A list of tokens from the target string, casefolded
getTokens = T.words . clean . T.toCaseFold
 where
  toDelete = ".'"
  clean :: T.Text -> T.Text
  clean = T.filter (`notElem` toDelete) . T.map cleanChar
  cleanChar :: Char -> Char
  cleanChar c | any ($ c) [isLower, isDigit, isSpace, (`elem` toDelete)] = c
              | otherwise = ' '

{- | Given the list of entries to be held by QuickSearch, return a HashMap
   keyed on tokens from the strings in the entries, where the associated
   HashMap value is the list of uids of entries containing the token.
-}
buildTokenPartitions
  :: (Hashable uid, Eq uid)
  => [Entry uid]  -- ^ List of entries
  -> HMap.HashMap Token (HSet.HashSet uid)  -- ^ A map of Token -> [uids]
buildTokenPartitions = tokenPartitions . map toTokenizedTuple

{- | Given a list of tokenized entries to be held by QuickSearch,
   return a HashMap keyed on the set of distinct tokens where the associated
   HashMap value is the list of uids of entries containing the token.
-}
tokenPartitions
  :: forall uid . (Hashable uid, Eq uid)
  => [([Token], uid)]  -- ^ List of tokenized entries
  -> HMap.HashMap Token (HSet.HashSet uid)  -- ^ A map of Token -> [uids]
tokenPartitions tokenizedEntries =
  HMap.fromList $ [(tok, allWith tok) | tok <- allTokens]
 where
  -- | Quick dedupe of a list. Does not preserve order.
  unstableNub :: [Token] -> [Token]
  unstableNub = HSet.toList . HSet.fromList
  allTokens = unstableNub . concatMap fst $ tokenizedEntries
  allWith :: (Hashable uid, Eq uid) => Token -> HSet.HashSet uid
  allWith token =
    HSet.fromList . map snd $ filter ((token `elem`) . fst) tokenizedEntries

{- | Given a target string and a Token HashMap, return the union of
   sets of uids associated with the tokens in the target string
-}
getSearchPartition
  :: (Hashable uid, Eq uid)
  => T.Text  -- ^ Target string
  -> HMap.HashMap Token (HSet.HashSet uid)
  -- ^ HashMap associating tokens with sets of uids
  -> HSet.HashSet uid  -- ^ The union of sets of associated uids.
getSearchPartition name tokenMap =
  let tokens = getTokens name
  in  HSet.unions $ map (fromMaybe HSet.empty . (`HMap.lookup` tokenMap)) tokens
