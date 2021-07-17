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

getTokens :: T.Text -> [Token]
getTokens = T.words . clean . T.toCaseFold
 where
  toDelete = ".'"
  clean :: T.Text -> T.Text
  clean = T.filter (`notElem` toDelete) . T.map cleanChar
  cleanChar :: Char -> Char
  cleanChar c | any ($ c) [isLower, isDigit, isSpace, (`elem` toDelete)] = c
              | otherwise = ' '

buildTokenPartitions
  :: (Hashable uid, Eq uid)
  => [(T.Text, uid)]
  -> HMap.HashMap Token (HSet.HashSet uid)
buildTokenPartitions = tokenPartitions . map (first getTokens)

tokenPartitions
  :: forall uid
   . (Hashable uid, Eq uid)
  => [([Token], uid)]
  -> HMap.HashMap Token (HSet.HashSet uid)
tokenPartitions entries = HMap.fromList $ map (id &&& allWith) allTokens
 where
  allTokens = nub . concatMap fst $ entries
  allWith :: (Hashable uid) => Token -> HSet.HashSet uid
  allWith token =
    HSet.fromList . map snd $ filter ((token `elem`) . fst) entries

getSearchPartition
  :: (Hashable uid, Eq uid)
  => T.Text
  -> HMap.HashMap Token (HSet.HashSet uid)
  -> HSet.HashSet uid
getSearchPartition name tokenMap =
  let tokens = getTokens name
  in  HSet.unions $ map (fromMaybe HSet.empty . (`HMap.lookup` tokenMap)) tokens
