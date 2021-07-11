{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module MakeFilter (buildTokenPartitions, getSearchPartition) where

import           Control.Arrow
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
buildTokenPartitions = tokenPartitions . map (first getTokens)

tokenPartitions :: [([Token], UID)] -> M.Map Token [UID]
tokenPartitions entries = M.fromList [(tok, allWith tok) | tok <- allTokens]
  where
    allTokens = nub . concatMap fst $ entries
    allWith :: Token -> [UID]
    allWith token = map snd $ filter ((token `elem`) . fst) entries

getSearchPartition :: T.Text -> M.Map Token [UID] -> [UID]
getSearchPartition name tokenMap =
  let tokens = getTokens name
  in nub . concatMap (fromMaybe [] . flip M.lookup tokenMap) $ tokens
