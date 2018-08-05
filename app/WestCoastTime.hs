module WestCoastTime
 ( getCaTime
 , isCaTime
 ) where

import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime (localDay, localTimeOfDay, todHour, todMin)
import Data.Time.Calendar (toGregorian)
import Data.Time.Zones (utcToLocalTimeTZ)
import Data.Time.Zones.All (tzByLabel, TZLabel (..))

-- | Returns the date in California as (year, month, day, hour, minute)
getTzTimeAt :: TZLabel -> UTCTime -> IO (Integer, Int, Int, Int, Int)
getTzTimeAt tzl utc = do
  let tz         = tzByLabel tzl
      localtime  = utcToLocalTimeTZ tz utc
      time       = localTimeOfDay localtime
      date       = localDay localtime
      (h, m)     = (todHour time, todMin time)
      (y, mo, d) = toGregorian date
  return (y, mo, d, h, m)

getCaTimeAt :: UTCTime -> IO (Integer, Int, Int, Int, Int)
getCaTimeAt = getTzTime America__Los_Angeles

getCaTime :: IO (Integer, Int, Int, Int, Int)
getCaTime = do
  now <- getCurrentTime
  getCaTimeAt now

-- | Returns whether it is the given time (hour, minute) in California
isCaTime :: Int -> Int -> IO (Maybe Bool)
isCaTime h m = do
  (_, _, _, caH, caM) <- getCaTime
  case (h >= 0 && h < 24 && m >= 0 && m < 60) of
    False -> return Nothing
    True  -> return $ Just $ h == caH && m == caM
