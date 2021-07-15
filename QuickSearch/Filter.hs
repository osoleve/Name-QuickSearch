{-# LANGUAGE ScopedTypeVariables #-}

module QuickSearch.Filter
  ( buildTokenPartitions
  , getSearchPartition
  )
where

import           Control.Arrow
import           Data.Char
import qualified Data.HashSet  as HSet
import           Data.List
import qualified Data.Map      as M
import           Data.Maybe
import qualified Data.Text     as T

type UID = Int
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

buildTokenPartitions :: [(T.Text, UID)] -> M.Map Token (HSet.HashSet UID)
buildTokenPartitions = tokenPartitions . map (first getTokens)

tokenPartitions :: [([Token], UID)] -> M.Map Token (HSet.HashSet UID)
tokenPartitions entries = M.fromList [ (tok, allWith tok) | tok <- allTokens ]
 where
  allTokens = nub . concatMap fst $ entries
  allWith :: Token -> HSet.HashSet UID
  allWith token =
    HSet.fromList . map snd $ filter ((token `elem`) . fst) entries

getSearchPartition
  :: T.Text -> M.Map Token (HSet.HashSet UID) -> HSet.HashSet UID
getSearchPartition name tokenMap =
  let tokens = getTokens name
  in  HSet.unions $ map (fromMaybe HSet.empty . flip M.lookup tokenMap) tokens
