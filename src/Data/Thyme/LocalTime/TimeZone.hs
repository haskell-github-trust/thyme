{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

#include "thyme.h"

module Data.Thyme.LocalTime.TimeZone where

import Prelude
import Control.DeepSeq
import Control.Lens
import Data.Data
import Data.Thyme.Calendar
import Data.Thyme.Clock
import Data.Thyme.Format.Internal
import qualified Data.Time as T
import GHC.Generics (Generic)

-- * Time zones

data TimeZone = TimeZone
    { timeZoneMinutes :: {-# UNPACK #-}!Int{-FIXME:Minutes-}
    , timeZoneSummerOnly :: !Bool
    , timeZoneName :: String
    } deriving (INSTANCES_USUAL)

instance NFData TimeZone

#if SHOW_INTERNAL
deriving instance Show TimeZone
#else
instance Show TimeZone where
    show tz@TimeZone {..} = if null timeZoneName
        then timeZoneOffsetString tz else timeZoneName
#endif

-- | Text representing the offset of this timezone, e.g. \"-0800\" or
-- \"+0400\" (like %z in 'formatTime')
{-# INLINEABLE timeZoneOffsetString #-}
timeZoneOffsetString :: TimeZone -> String
timeZoneOffsetString TimeZone {..} = sign : (shows02 h . shows02 m) "" where
    (h, m) = divMod offset 60
    (sign, offset) = if timeZoneMinutes < 0
        then ('-', negate timeZoneMinutes) else ('+', timeZoneMinutes)

-- | Create a nameless non-summer timezone for this number of minutes
minutesToTimeZone :: Int -> TimeZone
minutesToTimeZone m = TimeZone m False ""

-- | Create a nameless non-summer timezone for this number of hours
hoursToTimeZone :: Int -> TimeZone
hoursToTimeZone i = minutesToTimeZone (60 * i)

utc :: TimeZone
utc = TimeZone 0 False "UTC"

{-# INLINEABLE getTimeZone #-}
getTimeZone :: UTCTime -> IO TimeZone
getTimeZone t = thyme `fmap` T.getTimeZone (T.UTCTime day $ toSeconds dt) where
    day = T.ModifiedJulianDay (toInteger mjd)
    UTCTime (ModifiedJulianDay mjd) dt = t ^. utcTime
    thyme T.TimeZone {..} = TimeZone {..}

{-# INLINE getCurrentTimeZone #-}
getCurrentTimeZone :: IO TimeZone
getCurrentTimeZone = getCurrentTime >>= getTimeZone

