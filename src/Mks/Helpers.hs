{-# LANGUAGE NoImplicitPrelude #-}
module Mks.Helpers where

import Prelude hiding (id)

import Text.XHtml.Strict

id               = identifier
css_link l       = itag "link" ! [rel "stylesheet", thetype "text/css", href l]
ie_tag x         = primHtml ("<!--[if IE]>" ++ show x ++ "<![endif]-->")
ie6_tag x        = primHtml ("<!--[if lt IE 7]>" ++ show x ++ "<![endif]-->")
ie7_tag x        = primHtml ("<!--[if IE 7]>" ++ show x ++ "<![endif]-->") 
js_link l        = itag "script" ! [thetype "text/javascript", src l]
js_src s         = tag "script" ! [thetype "text/javascript"] << s
rss_link l       = itag "link" ! [rel "alternate", thetype "application/rss+xml", href l, title "RSS 2.0"]
favicon_link l   = itag "link" ! [rel "icon", thetype "image/png", href l]
meta_tag         = meta ! [httpequiv "Content-Type", content "text/html; charset=utf-8"]

div_id s         = thediv ! [id s]
div_class s      = thediv ! [theclass s]
div_class_id x y = thediv ! [theclass x, id y]

p_id s           = paragraph ! [id s]


-- Validation
xhtmlstrict_icon = paragraph << 
                    hotlink "http://validator.w3.org/check?uri=referer" 
                      << image ! [src "http://www.w3.org/Icons/valid-xhtml10"
                                 ,alt "Valid XHTML 1.0 Strict"
                                 ,height "31"
                                 ,width "88"
                                 ]
