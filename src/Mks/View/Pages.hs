module Mks.View.Pages where

import qualified Mks.Helpers as H
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((<<),(+++),(!))

import Bolt.Plugin.XHtmlTemplate.Layout
import Bolt.Plugin.HStringTemplate

import Mks.Model.Book

index  = pHstRenderPrimHtml "index"

about = pHstRenderPrimHtml "about"

forward = pHstRenderPrimHtml "forward"

terms = pHstRenderPrimHtml "terms"

-- {{{ Home
home :: [MmChapter] -> [MmChapter] -> X.Html
home unlocked locked = 
  H.div_class "home" << [
    X.h2 << "Unlocked chapters"
  , tipbox << "You are allowed to read the information below. Every week another chapter will be unlocked. You will be notified by email. Click the title to read the chapter"
  , _chapterList unlocked False
  , X.h2 << "Locked chapters"
  , tipbox << "These chapters are still locked, every week one will be unlocked"
  , _chapterList locked True
  ]

_chapterList chapters locked =
  H.div_class "chapterlist" << map (_chapter locked) chapters

_chapter locked c = 
  H.div_class chapterClass << [
    case locked of 
      False -> X.h3 << X.hotlink ("/chapter/" ++ (show $ mcOrd c)) << mcTitle c
      True -> X.h3 << mcTitle c
  , X.paragraph << ((take 156 $ msContent $ head $ mcIntroduction c) ++ "...")
  ]
  where 
    chapterClass = case locked of
      False -> "chapter"
      True -> "chapterLocked"
-- }}}
      
-- {{{ Chapter
chapter :: MmChapter -> X.Html
chapter c = 
  H.div_class "chapter" << [
    X.h2 << mcTitle c
  , X.h3 << "Introduction"
  , X.ordList ( map msContent $ mcIntroduction c )
  , X.h3 << "The information"
  , X.ordList ( map msContent $ mcContent c )
  , X.h3 << "Questions"
  , X.ordList ( map msContent $ mcQuestions c )
  ]
-- }}}

tipbox = H.div_class "tipbox"
