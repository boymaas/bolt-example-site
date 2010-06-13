module Mks.Model.Member where

import Bolt.Plugin.HDBC.PostgreSQL
import Bolt.Types

import Control.Applicative
import Control.Monad.Trans

import Data.Time

data MmMember = MmMember {
  mmId :: Integer
, mmEmail :: String
, mmPassword :: String
, mmRegistrationDate :: UTCTime
, mmUnlockedChapter :: Integer
}

memberColList = words "id email password regdate unlockedChapter"

mmLoadMembers :: BtBolt [MmMember]
mmLoadMembers = do
  members <- pHdbcSelect "member" memberColList
  return $ map _toMmMember members
    
_toMmMember [id,email,password,regdate,unlockedChapter] = MmMember (fsql id) (fsql email) (fsql password) (fsql regdate) (fsql unlockedChapter)

mmLoadMember ::  Integer -> BtBolt MmMember
mmLoadMember id = do
  [member] <- pHdbcSelectWhere "member" memberColList "id = ?" [tsql id]
  return $ _toMmMember member

mmDaysRegistered ::  MmMember -> BtBolt Integer
mmDaysRegistered member = do
  currentDay <- utctDay <$> liftIO getCurrentTime
  return $ diffDays currentDay regDay
  where
    regDay = utctDay $ mmRegistrationDate member

mmMaxChapters ::  MmMember -> BtBolt Integer
mmMaxChapters member = do
  days <- mmDaysRegistered member
  return . fromInteger $ 1 + ( days `div` 7 )

