{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
#include "thyme.h"

module Data.Thyme.LocalTime
    ( module Data.Thyme.LocalTime.TimeZone
    , module Data.Thyme.LocalTime.Internal
    , module Data.Thyme.LocalTime
    ) where

import Prelude
import Control.Lens
import Data.Thyme.Calendar
import Data.Thyme.Clock
import Data.Thyme.LocalTime.Internal
import Data.Thyme.LocalTime.TimeZone

{-# INLINE getZonedTime #-}
getZonedTime :: IO ZonedTime
getZonedTime = utcToLocalZonedTime =<< getCurrentTime

{-# INLINEABLE utcToLocalZonedTime #-}
utcToLocalZonedTime :: UTCTime -> IO ZonedTime
utcToLocalZonedTime time = do
    tz <- getTimeZone time
    return $ (tz, time) ^. zonedTime

-- * Lenses

LENS(TimeZone,timeZoneMinutes,Int)
LENS(TimeZone,timeZoneSummerOnly,Bool)
LENS(TimeZone,timeZoneName,String)

LENS(TimeOfDay,todHour,Hour)
LENS(TimeOfDay,todMin,Minute)
LENS(TimeOfDay,todSec,DiffTime)

LENS(LocalTime,localDay,Day)
LENS(LocalTime,localTimeOfDay,TimeOfDay)

LENS(ZonedTime,zonedTimeToLocalTime,LocalTime)
LENS(ZonedTime,zonedTimeZone,TimeZone)

