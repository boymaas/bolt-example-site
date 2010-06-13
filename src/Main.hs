module Main (main) where

import Bolt
import Bolt.Shared

import qualified Bolt.Plugin.HDBC.PostgreSQL as PPostgreSQL
import qualified Bolt.Plugin.HStringTemplate as PHStringTemplate
import qualified Bolt.Plugin.Routing as PRouting
import qualified Bolt.Plugin.Session.PostgreSQL as PSession
import qualified Bolt.Plugin.XHtmlTemplate as PXHtmlTemplate

import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.List.HT as L
import qualified Mks as PMks
import System.FilePath ((</>))

-- Lightning Bolts came out of the sky!!!

connectionString = "user=postgres dbname=mks host=localhost"

main
  = do initialiseDatabase 
       -- start serving boltpages
       bStartBolt
         [PSession.plugin connectionString
         ,PPostgreSQL.plugin connectionString
         ,PXHtmlTemplate.plugin 
         ,PHStringTemplate.plugin True "tmpl/"
         ,PMks.plugin
         ] []

       return ()

-- {{{ Initialise Database
initialiseDatabase = do
  conn <- connectPostgreSQL connectionString
  createIfNotExists conn ["member", "chapter", "section"]
  disconnect conn

createIfNotExists conn tablenames = do
  tables <- getTables conn
  mapM_ createTable $ filter (`notElem` tables) tablenames
  where
    createTable tableName = do
      sql <- readFile $ "db" </> (tableName ++ ".sql")
      mapM_ (\s -> run conn s []) $ L.chop (';' ==) sql
      commit conn
      return ()      
-- }}}
