{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module QuickSearch.Internal.Filter
    ( buildTokenPartitions
    , getSearchPartition
    , wordTokenize
    , toTokenizedTuple
    , Token
    , Entry(..)
    , entryName
    , entryUID
    , first
    ) where

import           Control.Arrow                  ( Arrow((&&&)) )
import           Data.Bifunctor                 ( Bifunctor(bimap, first) )
import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                )
import qualified Data.HashMap.Lazy             as HMap
import qualified Data.HashSet                  as HSet
import           Data.Hashable                  ( Hashable )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T

type Token = T.Text

-- | Structure associating a name with its unique identifier
newtype Entry name uid = Entry (name, uid)
  deriving newtype (Show, Eq)

instance Bifunctor Entry where
    bimap f g (Entry (name, uid)) = Entry (f name, g uid)

-- | Name accessor for an Entry
entryName :: Entry name uid -> name
entryName (Entry (name, _)) = name

-- | UID accessor for an Entry
entryUID :: Entry name uid -> uid
entryUID (Entry (_, uid)) = uid

{- | Turn a Data.Text.Text string into a list of casefolded tokens.
     Turns most non-Alphanum into spaces and
     deletes all periods and apostrophes.

     >>> wordTokenize ("Jane Smith-Walker, M.D."::T.Text)
     ["jane", "smith", "walker", "md"]
-}
wordTokenize
    :: T.Text  -- ^ The target string
    -> [Token]  -- ^ A list of tokens from the target string, casefolded
wordTokenize = T.words . T.toCaseFold . T.map symToSpace . T.filter (`notElem` ".'")
  where
    symToSpace :: Char -> Char
    symToSpace c | isAlphaNum c || isSpace c = c
                 | otherwise                 = ' '

{- | Convert an Entry T.Text uid to a tuple of ([wordTokenize name], uid)

     >>> toTokenizedTuple ("Jane Smith-Walker, M.D.", 1)::Entry
     (["jane", "smith", "walker", "md"], 1)
-}
toTokenizedTuple :: (Hashable uid, Eq uid) => Entry T.Text uid -> ([Token], uid)
toTokenizedTuple = wordTokenize . entryName &&& entryUID

{- | Given the list of entries to be held by QuickSearch, return a HashMap
   keyed on tokens from the strings in the entries, where the associated
   HashMap value is the list of uids of entries containing the token.
-}
buildTokenPartitions
    :: (Hashable uid, Eq uid)
    => [Entry T.Text uid]  -- ^ List of entries
    -> HMap.HashMap Token (HSet.HashSet uid)  -- ^ A map of Token -> [uids]
buildTokenPartitions = tokenPartitions . map toTokenizedTuple

{- | Given a list of tokenized entries to be held by QuickSearch,
   return a HashMap keyed on the set of distinct tokens where the associated
   HashMap value is the list of uids of entries containing the token.
-}
tokenPartitions
    :: forall uid
     . (Hashable uid, Eq uid)
    => [([Token], uid)]  -- ^ List of tokenized entries
    -> HMap.HashMap Token (HSet.HashSet uid)  -- ^ A map of Token -> [uids]
tokenPartitions tokEntries = foldr (\t m -> HMap.insert t (allWith t) m)
                                   HMap.empty
                                   allTokens
  where
    unstableNub :: [Token] -> [Token]
    -- ^ Quick dedupe of a list. Does not preserve order.
    unstableNub = HSet.toList . HSet.fromList
    allTokens   = unstableNub . concatMap fst $ tokEntries
    allWith :: (Hashable uid, Eq uid) => Token -> HSet.HashSet uid
    allWith token = HSet.fromList . map snd . filter ((token `elem`) . fst) $ tokEntries

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
    let tokens = wordTokenize name
    in  HSet.unions $ map (fromMaybe HSet.empty . (`HMap.lookup` tokenMap)) tokens
