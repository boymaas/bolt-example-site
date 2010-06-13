#!/usr/bin/runhaskell

import System
import Database.HDBC
import Database.HDBC.PostgreSQL
import Text.HTML.TagSoup
import Text.RegexPR
import Data.Maybe

import Control.Applicative

import qualified Bolt.Plugin.HDBC.SqlAbstraction as SqlA

main = do
  files <- getArgs
  importFiles files
  return ()

conn2db = do
  connectPostgreSQL "dbname=mks user=postgres"

importFiles orderedFileList = do
  conn <- conn2db
  mapM_ (createChapter conn) $ zip [1..] orderedFileList 
  commit conn

-- create a chapter from a file
createChapter :: Connection -> (Integer, String) -> IO ()
createChapter conn (chapterno, fpath) = do
  chid <- insertChapter conn chapterno

  tags <- canonicalizeTags . parseTags <$> readFile fpath

  let sections = partitions (~== TagOpen "h1" []) tags
  let [introduction,content,questions] = sections

  createSections conn chid "introduction" introduction
  createSections conn chid "content" content
  createSections conn chid "questions" questions
  
insertChapter conn chapterno = do
  print $ "creating chapter " ++ (show chapterno)
  execInsert conn "chapter" [cvp "ord" chapterno, cvp "title" ("CHAPTER - " ++ (show chapterno))]

createSections :: Connection -> Integer -> String -> [Tag] -> IO ()
createSections conn chid thetype content = do
  let elements = filter (\t -> length t > 0) 
                  . map stripWhiteSpace 
                  . filter doesntCountainPART
                  . map fromTagText 
                  . filter isTagText 
                  $ content
  mapM_ (insertSection conn chid thetype) $ zip [1..] elements
  where 
    doesntCountainPART = null . fst. fst . fromMaybe (("",("","")),[]) . matchRegexPR "PART"

insertSection :: Connection -> Integer -> String -> (Integer,String) -> IO Integer
insertSection conn chid thetype (number,content) = do
  print $ "creating section ("++ thetype ++") " ++ (show number)
  execInsert conn "section" [cvp "chapter_id" chid, cvp "typeofsection" thetype, cvp "ord" number, cvp "content" $ stripWhiteSpace content]

-- helpers
execInsert c t cvp = runInsert c $ SqlA.insert t cvp

runInsert :: (IConnection conn) => conn -> SqlA.InsertStatement -> IO Integer
runInsert conn (stmnt, values, table) = do
  SqlA.runSql conn (stmnt, values)
  r <- SqlA.query' conn ("select currval('"++ table ++"_id_seq');",[])          
  return . fromSql . head . head $ r
  
cvp a b = (a,toSql b)

stripWhiteSpace ::  String -> String
stripWhiteSpace = gsubRegexPR "(^\\d+\\.|^\\s*|\\s*$)" "" 
