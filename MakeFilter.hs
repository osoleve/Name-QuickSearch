{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module MakeFilter (buildTokenPartitions, getSearchPartition) where

import qualified Data.Bifunctor    as B
import           Data.Char
import           Data.List
import qualified Data.Map          as M
import           Data.Maybe
import qualified Data.Text         as T

type UID = Int
type Token = T.Text

getTokens :: T.Text -> [Token]
getTokens = T.words . clean
  where
    toDelete = ".'"::String
    clean :: T.Text -> T.Text
    clean = T.filter (`notElem` toDelete) . T.map cleanChar
    cleanChar :: Char -> Char
    cleanChar c
      | any ($ c) [isUpper, isLower, isDigit, isSpace, (`elem` toDelete)] = c
      | otherwise = ' '

buildTokenPartitions :: [(T.Text, UID)] -> M.Map Token [UID]
buildTokenPartitions = tokenPartitions . map (B.first getTokens)

tokenPartitions :: [([Token], UID)] -> M.Map Token [UID]
tokenPartitions entries = M.fromList [(tok, allWith tok) | tok <- allTokens]
  where
    allTokens = nub . concatMap fst $ entries
    allWith :: Token -> [UID]
    allWith token = map snd $ filter ((token `elem`) . fst) entries

dataPartitions :: forall a. (Ord a) => [(a, UID)] -> M.Map a [UID]
dataPartitions entries = M.fromList [(item, allWith item) | item <- allItems]
  where
    allItems::[a] = nub . map fst $ entries
    allWith :: a -> [UID]
    allWith item = map snd $ filter ((==item) . fst) entries

getSearchPartition :: T.Text -> M.Map Token [UID] -> [UID]
getSearchPartition name tokenMap =
  let tokens = getTokens name
  in nub . concatMap (fromMaybe [] . flip M.lookup tokenMap) $ tokens
