module Mks.Forms ( register, login
                , runFormState
                , Registration(..)
                , Login(..)
                ) where

import Control.Applicative
import Data.Monoid
import Network.CGI
import Text.Formlets

import Text.XHtml.Strict.Formlets (Form)
import qualified Text.XHtml.Strict.Formlets as F
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((<<),(+++))

import Bolt.Types (BtBolt)
import Bolt.Plugin.HDBC.PostgreSQL 
import Database.HDBC

import Control.Monad.Reader

data Registration = Registration { regUser :: String
                                 , regPass :: String }
                                 deriving Show

data Login = Login {
  logEmail :: String
}

-- notice the BtBolt specification here. So register
-- will work within our BtBolt connectors ;)
register ::  Form Html BtBolt Registration
register = Registration <$> uniqueEmail <*> passConfirmed

login :: Form Html BtBolt Login
login = Login <$> validatedLogin

-- check for unique email inside database
uniqueEmail :: Form Html BtBolt String
uniqueEmail = email caption `F.checkM` F.ensureM valid error where
  valid e = null <$> pHdbcQuery (statement, [tsql e])
  statement = "select * from member where email = ?"
  error = caption ++ " already exists in database"
  caption = "Email address"

-- check for validated login
validatedLogin = fst <$> loginCombo `F.checkM` F.ensureM valid error where
  loginCombo = (,) <$> email "Emailaddress" <*> pass "Password"
  valid (e,p) = not . null <$> pHdbcSelectWhere' "member" ["id"] "email = ? and password = ?" [tsql e,tsql p]
  error = "We cannot find this combination of email and password. Check `Caps Lock' and try again."

-- check if 2 passwords match .. return only first
passConfirmed :: (Applicative m,Monad m) => Form Html m String
passConfirmed = fst <$> passwords `F.check` F.ensure equal error where
  passwords = (,) <$> pass "Password" <*> pass "Password (confirm)"
  equal (a,b) = a == b
  error = "The entered passwords do not match!"

-- check email length
email ::  (Monad m) => [Char] -> Form Html m String
email caption = 
  input `F.check` F.ensure valid error where
    input = caption `label` F.input Nothing
    valid = (>=5). length
    error = caption ++ " must be five characters or longer."

-- check password length
pass :: (Applicative m,Monad m) => String -> Form Html m String
pass caption = input `F.check` F.ensure valid error where
    input = caption `label` F.password Nothing
    valid = (>=4). length
    error = caption ++ " must be four characters or longer."

-- plug essentially wraps the supplied xml with whatever you give it
label :: (X.HTML xml, Monad m, Monoid xml) => String -> Form xml m a -> Form Html m a
label l = F.plug (\xhtml -> X.p << (X.label << (l ++ ": ") +++ xhtml))

