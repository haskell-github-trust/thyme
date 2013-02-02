module Data.Thyme.LocalTime.TimeZone (
    -- * Time zones
      T.TimeZone (..)
    , T.timeZoneOffsetString
    , T.timeZoneOffsetString'
    , T.minutesToTimeZone
    , T.hoursToTimeZone
    , T.utc
    , T.getCurrentTimeZone
    , module Data.Thyme.LocalTime.TimeZone
    ) where

import Prelude
import Control.Lens
import Data.Micro
import Data.Thyme.Calendar
import Data.Thyme.Clock.Internal
import qualified Data.Time as T

getTimeZone :: UTCTime -> IO T.TimeZone
getTimeZone time = T.getTimeZone (T.UTCTime day dayTime) where
    day = T.ModifiedJulianDay (fromIntegral mjd)
    dayTime = fromRational $ dt ^/^ DiffTime (toMicro 1)
    UTCTime (ModifiedJulianDay mjd) dt = view utcTime time

