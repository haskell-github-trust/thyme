module Data.Thyme.LocalTime.TimeZone (
    -- * Time zones
      T.TimeZone (..)
    , T.timeZoneOffsetString
    , T.timeZoneOffsetString'
    , T.minutesToTimeZone
    , T.hoursToTimeZone
    , T.utc
    , module Data.Thyme.LocalTime.TimeZone
    ) where

import Prelude
import Control.Lens
import Data.Thyme.Calendar
import Data.Thyme.Clock
import qualified Data.Time as T

{-# INLINEABLE getTimeZone #-}
getTimeZone :: UTCTime -> IO T.TimeZone
getTimeZone t = T.getTimeZone $ T.UTCTime day (toSeconds dt) where
    day = T.ModifiedJulianDay (toInteger mjd)
    UTCTime (ModifiedJulianDay mjd) dt = t ^. utcTime

{-# INLINE getCurrentTimeZone #-}
getCurrentTimeZone :: IO T.TimeZone
getCurrentTimeZone = getCurrentTime >>= getTimeZone

