module Mks.Model.Book where

import Bolt.Plugin.HDBC.PostgreSQL
import Bolt.Types

import Control.Applicative

import Database.HDBC
import Data.List

data MmBook = MmBook {
  mbChapters :: [MmChapter]
}

data MmChapter = MmChapter {
  mcId :: Integer
, mcOrd :: Integer
, mcTitle :: String
, mcIntroduction :: [MmSection]
, mcContent :: [MmSection]
, mcQuestions :: [MmSection]
}

data MmSection = MmSection {
  msId :: Integer
, msOrd :: Integer
, msContent :: String
}

mmLoadChapters :: [Integer] -> BtBolt [MmChapter]
mmLoadChapters [] = return []
mmLoadChapters ids = do
  chapters <- pHdbcQuery' ("select id,ord,title from chapter where " ++ idlist ids ++ " order by ord asc", map tsql ids)
  mapM (\ [id, ord, title] -> mmLoadChapter (fsql id) (fsql ord) (fsql title)) chapters
  where
    idlist = concat . intersperse " OR " . map (\_ -> "ord = ?")

mmLoadBook ::  BtBolt MmBook
mmLoadBook = do
  chapters <- pHdbcQuery' ("select id,ord,title from chapter order by ord asc", [])
  MmBook <$> mapM (\ [id, ord, title] -> mmLoadChapter (fsql id) (fsql ord) (fsql title)) chapters

mmLoadChapter :: Integer -> Integer -> String -> BtBolt MmChapter
mmLoadChapter chapter_id ord title = do
  MmChapter chapter_id ord title 
           <$> (mmLoadSections chapter_id "introduction")
           <*> (mmLoadSections chapter_id "content") 
           <*> (mmLoadSections chapter_id "questions")

mmLoadSections :: Integer -> String -> BtBolt [MmSection]
mmLoadSections chapter_id thetype = do
  transform <$> pHdbcSelectWhere' "section" ["id","ord","content"] "chapter_id = ? AND typeofsection = ?" [tsql chapter_id, tsql thetype] >>= return
  where 
    transform = map (\[id, ord, content] -> MmSection (fsql id) (fsql ord) (fsql content))
