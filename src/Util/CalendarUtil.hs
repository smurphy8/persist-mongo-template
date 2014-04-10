{-# LANGUAGE OverloadedStrings #-}
module Util.CalendarUtil where
-- import qualified Database.MongoDB as MDB
import Data.Aeson.Types
import System.Locale
import Data.Time
import Data.Time.Calendar.WeekDate
import Persist.Mongo.Settings
import Control.Applicative
import Yesod hiding (runDB)


-- | Helper functions to explicity do type conversion 

isOnDuty :: UserId -> UTCTime -> IO Bool
isOnDuty uid time = do
  let day = utctDay time
  let weekDay = (third $ toWeekDate day) - 1
  let diffTime = utctDayTime time
  mcalObj <- runDB $ getBy $ UniqueUserId uid
  case entityVal <$> mcalObj of
    Nothing -> return False
    (Just (CalendarWidget _ _ days hours mins durations _)) -> return $ isWithinTime timeList weekDay diffTime
      where
        hourSecs = map (*3600) hours
        minSecs = map (*60) mins
        secs = zipWith (+) hourSecs minSecs
        onDutyDiffTimes = map secondsToDiffTime (map toInteger secs)
        diffTimeDurations = map secondsToDiffTime (map toInteger durations)
        timeList = zip3 days onDutyDiffTimes diffTimeDurations

isWithinTime :: [(Int, DiffTime, DiffTime)] -> Int -> DiffTime -> Bool
isWithinTime ((day, diffStart, diffDuration):[]) targetDay targetDiffTime = day == targetDay && diffStart <= targetDiffTime && diffStart + diffDuration >= targetDiffTime
isWithinTime ((day, diffStart, diffDuration):list) targetDay targetDiffTime = day == targetDay && diffStart <= targetDiffTime && diffStart + diffDuration >= targetDiffTime || isWithinTime list targetDay targetDiffTime
isWithinTime [] _ _ = False

third :: (a, b, c) -> c
third (_, _, t) = t
