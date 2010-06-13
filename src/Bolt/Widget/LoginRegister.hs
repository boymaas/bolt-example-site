-- | Adds login and register functionality to application
-- to configure this plugin one has to supply only:
-- a) tablename
-- b) email column
-- c) password column
--
-- This widget works with:
--  Bolt.Plugin.XHtmlTemplate
--  Bolt.Plugin.HDBC.PostgreSQL
--  Bolt.Forms
--  Bolt.Plugin.Session.PostgreSQL
module Bolt.Widget.LoginRegister where

-- | handles the registerForm, that is validating and rendering it
-- when all is ok, it will redirect to specified url
--
-- if registration validates it will store new user details in the table and store
-- the member id in the session under boltWidgetLoginRegister_MemberId
--
-- validates means, email doesn't exists yet in the database and all formfields
-- are filled correctly
--
-- rendering means storing XHtml in the global layout state with label "registerForm"
--
-- errors will be stored as XHtml in the global layour state with lable "registerFormErrors"
registerForm label  -- ^ the xhtml function to embed the formfields in
             prefix -- ^ the prefix of the inputfields names
  = undefined

-- | handles the loginForm, that is validating and rendering it
-- when all is ok, it will redirect to the specified url
--
-- if login validates, that is, email and password match with table in database, member id will
-- be stored in the session under boltWidgetLoginRegister_MemberId
--
-- rendering means storing XHtml in the global layout state with label "loginForm"
--
-- errors will be stored as XHtml in the global layour state with lable "loginFormErrors"
loginForm label   -- ^ the xhtml function to embed the formfields in
          prefix  -- ^ the prefix of the inputfields names
  = undefined
