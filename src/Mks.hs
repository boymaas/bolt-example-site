module Mks where 

import Control.Applicative
import Control.Monad
import Data.Maybe
import Network.CGI

import Bolt.Plugin.Base
import Bolt.Plugin.XHtmlTemplate
import Bolt.Plugin.Session.PostgreSQL
import Bolt.Plugin.Routing (prParams)
import Bolt.Plugin.HDBC.PostgreSQL
import Bolt.Plugin.HStringTemplate hiding (tv)
import Bolt.Forms

import qualified Bolt.Widget.LoginRegister as BwLoginRegister

import qualified Mks.Forms as F
import qualified Mks.View.Layout as V
import qualified Mks.View.Pages as P
import qualified Mks.Model.Book as M
import qualified Mks.Model.Member as M

-- {{{ Plugin Definition
plugin = 
  emptyPlugin { 
    -- pre and postrequest handlers
    pbPdHookPreRequest = Just _mksPreRequest
    -- routes
  , pbPdRoutes = [ ("/register", layout P.index)
                  ,("/login", layout P.index)
                  ,("/logout", logout)
                  ,("/home", home)
                  ,("/about", layout P.about)
                  ,("/forward", layout P.forward)
                  ,("/terms", layout P.terms)
                  ,("/chapter/(\\d+)", chapter)
                --,("", function)
                  ,("/.*", layout P.index) 
                 ]
  }
-- }}}

-- {{{ Hooks
_mksPreRequest ::  BtBolt ()
_mksPreRequest = do
  -- set some standard variables
  loggedIn <- isLoggedIn
  googleAd <- pHstRenderPrimHtml "googlead"
  googleAnalytics <- pHstRenderPrimHtml "googleAnalytics"
  pxtTmplVarMany [
      tv "title" "Master Key System by Charles F Haanel"
    , tv "description" "Go through the original mail correspondence course of the master key system. Register and change your mind."
    , tv "keywords" "Master Key System,The Secret,ChangeMeditation"
    , tv "googlead" googleAd
    , tv "googleAnalytics" googleAnalytics
    , tv "loggedIn" loggedIn
    ]
  return ()

_mksPostRequest ::  BtBolt ()
_mksPostRequest = return ()
-- }}}

-- {{{ Layout / Main index page
layout content = do
  params <- getInputs
  bfHandleForm F.register "register" params registerSuccess registerFail $ \html -> do
    pxtTmplVar "registerForm" html
    return ()

  bfHandleForm F.login "login" params loginSuccess loginFail $ \html -> do
    pxtTmplVar "loginForm" html
    return ()

  pxtContent # content

  pxtRenderOutput V.layout 
  return ()

-- }}}
          
-- {{{ Register

registerSuccess regInfo = do
  createMember regInfo >>= loginAndRedirectToHome >> pHdbcCommit
  return ()

registerFail errors = do
  feedback "There where some complications with the form"
  pxtTmplVar "registerErrors" errors
  return ()
  
-- }}}

-- {{{ Login

logout = do
  pSSessionClear "memberId"
  redirect "/"
  return ()

loginSuccess (F.Login email) = do
  -- check user credentials
  -- since it passed the form
  -- we know he's in the database
  memberId <- fsql . head . head <$> pHdbcSelectWhere' "member" ["id"] "email = ?" [tsql email]
  loginAndRedirectToHome $ memberId
  return ()

loginFail errors = do
  feedback "We cannot find your membership details. Check your caps lock, and try again."
  pxtTmplVar "loginErrors" errors
  return ()

feedback :: String -> BtBolt ()
feedback f = do
  pxtTmplVar "feedback" f

-- functionality
createMember :: F.Registration -> BtBolt Integer
createMember (F.Registration email password) = do
  -- create member in database (HDBC plugin?)
  -- return the new identity
  pHdbcInsert "member" [cvp "email" email, cvp "password" password]

loginAndRedirectToHome :: Integer -> BtBolt ()
loginAndRedirectToHome memberId = do
  -- set session
  pSSessionStore "memberId" $ show memberId

  -- redirect (look for redirect function)
  redirect "/home"
  return ()

isLoggedIn = fromMaybe "" <$> pSSessionGet "memberId" 
-- }}}

-- {{{ Home
home = whenLoggedIn $ do
  member <- getLoggedInMember
  lastChapter <- M.mmMaxChapters member 

  -- read locked and unlocked chapters
  let (unLocked, locked) = break (> lastChapter) [1 .. 24]
  unLockedChapters <- M.mmLoadChapters unLocked
  lockedChapters <- M.mmLoadChapters locked

  --  set content to Home view
  pxtContent $ P.home unLockedChapters lockedChapters

  -- chapters are only clickable if user has waited long enough
  pxtRenderOutput V.layout 
-- }}}

-- {{{ Chapter
chapter = whenLoggedIn $ do
  p <- read . head <$> prParams
  c <- head <$> M.mmLoadChapters [p]

  -- make sure they don't read locked
  -- chapters
  lastChapter <- M.mmMaxChapters # getLoggedInMember
  case p <= lastChapter of
    True -> renderChapter c
    False -> redirect "/home" >> return ()

  where 
    renderChapter c = do
      pxtContent $ P.chapter c
      pxtRenderOutput V.layout
      return ()
-- }}}

-- {{{ Helpers
whenLoggedIn block = do
  s <- pSSessionExists "memberId"
  case s of
    True -> block
    False -> redirect "/" >> return ()

getLoggedInMember = do
  memberId <- fromJust <$> pSSessionGet "memberId"
  M.mmLoadMember (read memberId)

(#) a b = do 
  b' <- b
  a b'
-- }}}
