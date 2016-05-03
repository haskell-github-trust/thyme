{-# LANGUAGE ForeignFunctionInterface #-}

#ifndef mingw32_HOST_OS
#include <sys/time.h>
#endif

-- | <https://en.wikipedia.org/wiki/Unix_time POSIX time>
module Data.Thyme.Clock.POSIX
    ( posixDayLength
    , POSIXTime
    , posixTime
    , getPOSIXTime

    -- * Compatibility
    , posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    ) where

import Prelude
import Control.Lens
import Data.AdditiveGroup
import Data.Thyme.Internal.Micro
import Data.Thyme.Clock.Internal

#ifdef mingw32_HOST_OS
import System.Win32.Time
#else
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable
#endif

-- | The nominal (ignoring leap seconds) time difference since midnight
-- 1970-01-01, the Unix epoch. Equvialent to a normalised
-- @<http://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html struct timeval>@.
type POSIXTime = NominalDiffTime

-- | "Control.Lens.Iso" between 'UTCTime' and 'POSIXTime'.
--
-- @
-- > 'getPOSIXTime'
-- 1459515013.527711s
-- > 'review' 'posixTime' '<$>' 'getPOSIXTime'
-- 2016-01-01 12:50:45.588729 UTC
-- @
{-# INLINE posixTime #-}
posixTime :: Iso' UTCTime POSIXTime
posixTime = iso (\ (UTCRep t) -> t ^-^ unixEpoch)
        (UTCRep . (^+^) unixEpoch) where
    unixEpoch = review microseconds $
        {-ModifiedJulianDay-}40587 * {-posixDayLength-}86400000000

-- | Return the current system POSIX time via
-- @<http://www.gnu.org/software/libc/manual/html_node/High_002dResolution-Calendar.html gettimeofday>@,
-- or @getSystemTimeAsFileTime@ on Windows.
-- 
-- See also 'Data.Thyme.Clock.getCurrentTime', 'Data.Thyme.LocalTime.getZonedTime'.
{-# INLINE getPOSIXTime #-}
getPOSIXTime :: IO POSIXTime
#ifdef mingw32_HOST_OS

-- On Windows, the equlvalent of POSIX time is ‘file time’, defined as
-- the number of 100-nanosecond intervals that have elapsed since
-- 12:00 AM January 1, 1601 (UTC). We can convert this into a POSIX
-- time by adjusting the offset to be relative to the POSIX epoch.
getPOSIXTime = do
    FILETIME ft <- System.Win32.Time.getSystemTimeAsFileTime
    return . NominalDiffTime . Micro . fromIntegral $
        quot ft 10 - 11644473600000000{-ftEpoch ^. microseconds-}
--  ftEpoch = utcTime # UTCTime (gregorian # YearMonthDay 1601 1 1) zeroV

#else

getPOSIXTime = allocaBytes #{size struct timeval} $ \ ptv -> do
    throwErrnoIfMinus1_ "gettimeofday" $ gettimeofday ptv nullPtr
    CTime sec <- #{peek struct timeval, tv_sec} ptv
    CSUSeconds usec <- #{peek struct timeval, tv_usec} ptv
    return . NominalDiffTime . Micro $
        1000000 * fromIntegral sec + fromIntegral usec

foreign import ccall unsafe "time.h gettimeofday"
    gettimeofday :: Ptr () -> Ptr () -> IO CInt

#endif

------------------------------------------------------------------------

-- | Construct a 'UTCTime' from a 'POSIXTime'.
--
-- @
-- 'posixSecondsToUTCTime' = 'review' 'posixTime'
-- 'posixSecondsToUTCTime' t ≡ 'posixTime' 'Control.Lens.#' t
-- @
{-# INLINE posixSecondsToUTCTime #-}
posixSecondsToUTCTime :: POSIXTime -> UTCTime
posixSecondsToUTCTime = review posixTime

-- | Convert a 'UTCTime' to a 'POSIXTime'.
--
-- @
-- 'utcTimeToPOSIXSeconds' = 'view' 'posixTime'
-- @
{-# INLINE utcTimeToPOSIXSeconds #-}
utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
utcTimeToPOSIXSeconds = view posixTime

