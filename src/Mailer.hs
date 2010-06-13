module Main where

import Control.Monad.Trans
import Control.Monad
import Control.Applicative
import Network.Email.Sendmail
import Bolt
import Bolt.Types
import qualified Bolt.Plugin.HStringTemplate as PHst
import qualified Bolt.Plugin.HDBC.PostgreSQL as PSql

import Bolt.Plugin.HStringTemplate hiding (plugin)
import Bolt.Plugin.HDBC.PostgreSQL hiding (plugin)

import Database.HDBC

import Mks.Model.Member

main = do
  bRunBoltScript [PSql.plugin "user=postgres dbname=mks host=localhost",
                  PHst.plugin True "tmpl/"
                 ] mailer
  return ()

(#) ::  (Monad m) => (t -> m b) -> m t -> m b
(#) a b = do 
  b' <- b
  a b'

infixr 6 #

mailer ::  BtBolt ()
mailer = do
  mapM_ sendReminderMail # filterM membersWithNewUnlockedChapters # mmLoadMembers
  where
    membersWithNewUnlockedChapters :: MmMember -> BtBolt Bool
    membersWithNewUnlockedChapters m = do
      maxChapters <- mmMaxChapters m 
      return $ maxChapters > mmUnlockedChapter m

sendReminderMail :: MmMember -> BtBolt ()
sendReminderMail member@(MmMember {mmEmail=email, mmId=id}) = do
  -- render mail
  mailtext <- pHstRenderTmplAttr "mails/newChapterUnlocked" [tv "email" email, tv "link" "http://mks.tagged.nl"]
  -- send da mail
  liftIO $ sendmail Nothing [email] mailtext
  -- update status of member
  maxChapters <- mmMaxChapters member
  pHdbcUpdate "member" [cvp "unlockedChapter" maxChapters] "id = ?" [tsql id]
  pHdbcCommit
