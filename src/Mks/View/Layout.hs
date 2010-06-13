module Mks.View.Layout where

import Control.Monad
import qualified Mks.Helpers as H
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((<<),(+++),(!))

import Bolt.Plugin.XHtmlTemplate.Layout

-- {{{ Layout
layout :: XHtmlLayout
layout s = 
  header s +++ body s 

header :: XHtmlLayout
header s = 
  X.header << [
     H.meta_tag
    ,X.thetitle << s # "title"
    ,X.meta ! [X.name "keywords", X.content (s #$ "keywords")]
    ,X.meta ! [X.name "description", X.content (s #$ "description")]
    ,H.css_link "/default.css"
    ,H.favicon_link "/favicon_haanel.png"
    ]
  
body s = 
  X.body << H.div_id "container" << (menu s +++ logo +++ page s +++ footer +++ googlead s +++ googleAnalytics s )

googlead s = H.div_id "googleads" << ( s # "googlead" )

googleAnalytics s = s # "googleAnalytics"

footer =
  H.div_id "footer" << footer'

footer' = 
  H.p_id "legal" << 
    ("Copyright " +++ X.copyright +++ "  2009 Innovita. All Rights Reserved.")
  +++
  H.xhtmlstrict_icon
  +++
  H.p_id "links" << [
    X.hotlink "/terms" << "Terms of Use"
  ]
-- }}}

-- {{{ Logo
logo =
  H.div_id "logo" << [
      X.h1 << X.hotlink "#" << X.thespan << "System"
     ,X.h2 << X.hotlink "#" << X.thespan << "By Charles F. Haanel"
     ]
-- }}}

-- {{{ Menu
menu s =
  H.div_id "menu" << 
     X.unordList ( [
      X.hotlink "/home" << "Home"
     --,X.hotlink "#" << "Forum"
     ,X.hotlink "/forward" << "Forward"
     ,X.hotlink "/about" << "About"
     ]
     ++
     case null (s #$ "loggedIn") of
       True -> []
       False -> [X.hotlink "/logout" << "Logout"]
     )
-- }}}

-- {{{ Page
page s =
  H.div_id "page" << [ 
    feedback s
  , page' s
  ]

feedback s = cond (s #$ "feedback") << H.div_id "feedback" << s # "feedback"

page' s = 
  content s 
  +++ 
  sidebar s
  +++
  H.div_id "extra" ! [X.thestyle "clear: both;"] << ""

content s = 
  H.div_id "content" << (
    s # "content"
    )
-- }}}

-- {{{ Sidebar
sidebar s = 
  H.div_id "sidebar" << sidebar' s


sidebar' s = 
  notCond (s #$ "loggedIn") << [
    boxed "Client Login" "login" <<
      loginForm s
    +++
    boxed "Register" "register" <<
      registerForm s
  ]
  +++
  boxed "Contents" "reasons" <<
    X.unordList [
      "The secret behind the secret"
     ,"24 weekly parts that will change your mind"
     ,"Aquire the power to overcome any obstacle"
    ] ! [X.theclass "list1"]

boxed :: String -> String -> X.Html -> X.Html
boxed title_ id_ c = 
  H.div_class_id "boxed" id_ <<
    H.div_class "title" <<
      X.h2 << title_
  +++
  H.div_class "content" << c

-- }}}

-- {{{ Forms
registerForm s = do 
  let (formErrors, formHtml) = (s ## "registerErrors", s # "registerForm")
  X.paragraph ! [X.theclass "termsatform"] << ("Registration implies conforming with the " +++ (X.hotlink "/terms" << "'Terms of Use'"))
    +++ renderForm "/register" "Register" formHtml formErrors
    

loginForm s = do 
  let (formErrors, formHtml) = (s ## "loginErrors", s # "loginForm")
  renderForm "/login" "Login" formHtml formErrors

renderForm action submitText formHtml formErrors = do
  H.div_class "form" << 
    [ cond formErrors << H.div_class "errors" <<
        X.unordList formErrors
    , X.form ! [X.method "post", X.action action] << (formHtml +++ X.input ! [X.theclass "submit", X.thetype "submit", X.value submitText])
    ]

-- }}}

-- {{{ Helpers
cond :: [a] -> X.Html -> X.Html
cond a html = case null a of 
  False -> html
  True -> X.noHtml

notCond ::  [a] -> X.Html -> X.Html
notCond a html = case null a of
  True -> html
  False -> X.noHtml
-- }}}
