{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "thyme.h"
#if HLINT
#include "cabal_macros.h"
#endif

{-|
Time Zones.
-}
module Data.Thyme.LocalTime
    ( Hour, Minute
    , module Data.Thyme.LocalTime
    ) where

import Prelude hiding ((.))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Arrow
import Control.Category hiding (id)
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Bits
import Data.Data
import Data.Hashable
import Data.Int
import Data.Thyme.Internal.Micro
import Data.Thyme.Calendar
import Data.Thyme.Calendar.Internal
import Data.Thyme.Clock
import Data.Thyme.Clock.Internal
import Data.Thyme.Format.Internal
import qualified Data.Time as T
#if __GLASGOW_HASKELL__ == 704
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
#endif
import Data.Vector.Unboxed.Deriving
import Data.VectorSpace
import GHC.Generics (Generic)
import System.Random
import Test.QuickCheck hiding ((.&.))

-- | Hours duration.
type Hours = Int
-- | Minutes duration.
type Minutes = Int

------------------------------------------------------------------------
-- * Time zones

-- | Description of one Time Zone.
--
-- A 'TimeZone' is a whole number of minutes offset from UTC, together with a
-- name and a "just for summer" flag.
data TimeZone = TimeZone
    { timeZoneMinutes :: {-# UNPACK #-}!Minutes
        -- ^ The number of minutes offset from UTC. Positive means local time
        -- will be later in the day than UTC.
        --
        -- 'Bounded' instance range /-12 × 60/ ≤ 'timeZoneMinutes' ≤ /13 × 60/
    , timeZoneSummerOnly :: !Bool
        -- ^ Is this a summer-only (i.e. daylight savings) Time Zone?
    , timeZoneName :: String
        -- ^ The name of the zone, typically a three- or four-letter acronym.
        --
        -- 'Bounded' instance range /AAAA/ ≤ 'timeZoneName' ≤ /ZZZZ/
    } deriving (INSTANCES_USUAL)

instance Hashable TimeZone
instance NFData TimeZone

#if SHOW_INTERNAL
deriving instance Show TimeZone
#else
instance Show TimeZone where
    show tz@TimeZone {..} = if null timeZoneName
        then timeZoneOffsetString tz else timeZoneName
#endif

instance Bounded TimeZone where
    minBound = TimeZone (-12 * 60) minBound "AAAA"
    maxBound = TimeZone (13 * 60) maxBound "ZZZZ"

instance Random TimeZone where
    randomR (l, u) g0 = (TimeZone minutes summer name, g3) where
        (minutes, g1) = randomR (timeZoneMinutes l, timeZoneMinutes u) g0
        (summer, g2) = randomR (timeZoneSummerOnly l, timeZoneSummerOnly u) g1
        -- slightly dubious interpretation of ‘range’
        (name, g3) = foldr randChar ([], g2) . take 4 $ zip
            (timeZoneName l ++ "AAAA") (timeZoneName u ++ "ZZZZ")
        randChar nR (ns, g) = (: ns) `first` randomR nR g
    random = randomR (minBound, maxBound)

instance Arbitrary TimeZone where
    arbitrary = choose (minBound, maxBound)
    shrink tz@TimeZone {..}
        = [ tz {timeZoneSummerOnly = s} | s <- shrink timeZoneSummerOnly ]
        ++ [ tz {timeZoneMinutes = m} | m <- shrink timeZoneMinutes ]
        ++ [ tz {timeZoneName = n} | n <- shrink timeZoneName ]

instance CoArbitrary TimeZone where
    coarbitrary (TimeZone m s n)
        = coarbitrary m . coarbitrary s . coarbitrary n

-- | Text representing the offset of this timezone, e.g. \"-0800\" or
-- \"+0400\" (like %z in 'Data.Thyme.Format.formatTime')
{-# INLINEABLE timeZoneOffsetString #-}
timeZoneOffsetString :: TimeZone -> String
timeZoneOffsetString TimeZone {..} = sign : (shows02 h . shows02 m) "" where
    (h, m) = divMod offset 60
    (sign, offset) = if timeZoneMinutes < 0
        then ('-', negate timeZoneMinutes) else ('+', timeZoneMinutes)

-- | Text representing the offset of this timezone in ISO 8601 style,
-- e.g. \"-08:00\" or
-- \"+04:00\" (like %N in 'Data.Thyme.Format.formatTime')
{-# INLINEABLE timeZoneOffsetStringColon #-}
timeZoneOffsetStringColon :: TimeZone -> String
timeZoneOffsetStringColon TimeZone {..} =
    sign : (shows02 h . (":"++) . shows02 m) "" where
    (h, m) = divMod offset 60
    (sign, offset) = if timeZoneMinutes < 0
        then ('-', negate timeZoneMinutes) else ('+', timeZoneMinutes)

-- | Create a nameless non-summer timezone for this number of minutes
minutesToTimeZone :: Minutes -> TimeZone
minutesToTimeZone m = TimeZone m False ""

-- | Create a nameless non-summer timezone for this number of hours
hoursToTimeZone :: Hours -> TimeZone
hoursToTimeZone i = minutesToTimeZone (60 * i)

-- | The UTC (Zulu) Time Zone.
--
-- @
-- utc = 'TimeZone' 0 'False' \"UTC\"
-- @
utc :: TimeZone
utc = TimeZone 0 False "UTC"

-- | Get the local system Time Zone for a given time (varying as per
-- summertime adjustments).
--
-- Performed by
-- <https://www.gnu.org/software/libc/manual/html_node/Broken_002ddown-Time.html localtime_r>
-- or a similar call.
{-# INLINEABLE getTimeZone #-}
getTimeZone :: UTCTime -> IO TimeZone
getTimeZone t = thyme `fmap` T.getTimeZone (T.UTCTime day $ toSeconds dt) where
    day = T.ModifiedJulianDay (toInteger mjd)
    UTCView (ModifiedJulianDay mjd) dt = t ^. utcTime
    thyme T.TimeZone {..} = TimeZone {..}

-- | Get the current local system Time Zone.
--
-- @
-- 'getCurrentTimeZone' ≡ 'getCurrentTime' >>= 'getTimeZone'
-- @
--
-- ==== Examples
--
-- @
-- > 'getCurrentTimeZone'
--   JST
-- @
{-# INLINE getCurrentTimeZone #-}
getCurrentTimeZone :: IO TimeZone
getCurrentTimeZone = getCurrentTime >>= getTimeZone

------------------------------------------------------------------------
-- * Time of day

-- | Time of day in hour, minute, second.
data TimeOfDay = TimeOfDay
    { todHour :: {-# UNPACK #-}!Hour
        -- ^ Hour.
    , todMin :: {-# UNPACK #-}!Minute
        -- ^ Minute.
    , todSec :: {-# UNPACK #-}!DiffTime
        -- ^ Second.
    } deriving (INSTANCES_USUAL)

derivingUnbox "TimeOfDay" [t| TimeOfDay -> Int64 |]
    [| \ TimeOfDay {..} -> fromIntegral (todHour .|. shiftL todMin 8)
        .|. shiftL (todSec ^. microseconds) 16 |]
    [| \ n -> TimeOfDay (fromIntegral $ n .&. 0xff)
        (fromIntegral $ shiftR n 8 .&. 0xff) (microseconds # shiftR n 16) |]

instance Hashable TimeOfDay
instance NFData TimeOfDay

#if SHOW_INTERNAL
deriving instance Show TimeOfDay
#else
instance Show TimeOfDay where
    showsPrec _ (TimeOfDay h m (DiffTime s))
            = shows02 h . (:) ':' . shows02 m . (:) ':'
            . shows02 (fromIntegral si) . frac where
        (si, Micro su) = microQuotRem s (Micro 1000000)
        frac = if su == 0 then id else (:) '.' . fills06 su . drops0 su
#endif

instance Bounded TimeOfDay where
    minBound = TimeOfDay 0 0 zeroV
    maxBound = TimeOfDay 23 59 (microseconds # 60999999)

instance Random TimeOfDay where
    randomR = randomIsoR timeOfDay
    random = first (^. timeOfDay) . random

instance Arbitrary TimeOfDay where
    arbitrary = do
        h <- choose (0, 23)
        m <- choose (0, 59)
        let DiffTime ml = minuteLength h m
        TimeOfDay h m . DiffTime <$> choose (zeroV, pred ml)
    shrink tod = view timeOfDay . (^+^) noon
            <$> shrink (timeOfDay # tod ^-^ noon) where
        noon = timeOfDay # midday -- shrink towards midday

instance CoArbitrary TimeOfDay where
    coarbitrary (TimeOfDay h m s)
        = coarbitrary h . coarbitrary m . coarbitrary s

-- | The maximum possible length of a minute. Always /60/, except at /23:59/,
-- because of leap seconds.
--
-- @
-- 'minuteLength' 23 59 = 61s
-- 'minuteLength' _  _  = 60s
-- @
{-# INLINE minuteLength #-}
minuteLength :: Hour -> Minute -> DiffTime
minuteLength h m = fromSeconds' $ if h == 23 && m == 59 then 61 else 60

-- | Hour zero, midnight.
midnight :: TimeOfDay
midnight = TimeOfDay 0 0 zeroV

-- | Hour twelve, noon.
midday :: TimeOfDay
midday = TimeOfDay 12 0 zeroV

-- | Construct a 'TimeOfDay' from hour, minute, second.
--
-- Returns 'Nothing' if these constraints are not satisfied:
--
-- * /0 ≤ hour ≤ 23/
-- * /0 ≤ minute ≤ 59/
-- * /0 ≤ second < 'minuteLength' hour minute/
{-# INLINE makeTimeOfDayValid #-}
makeTimeOfDayValid :: Hour -> Minute -> DiffTime -> Maybe TimeOfDay
makeTimeOfDayValid h m s = TimeOfDay h m s
    <$ guard (0 <= h && h <= 23 && 0 <= m && m <= 59)
    <* guard (zeroV <= s && s < minuteLength h m)

-- | "Control.Lens.Iso" between 'DiffTime' and 'TimeOfDay'.
--
-- ==== Examples
--
-- @
-- > 'fromSeconds'' 100 '^.' 'timeOfDay'
--   00:01:40
-- @
--
-- @
-- > 'timeOfDay' 'Control.Lens.Review.#' 'TimeOfDay' 0 1 40
--   100s
-- @
{-# INLINE timeOfDay #-}
timeOfDay :: Iso' DiffTime TimeOfDay
timeOfDay = iso fromDiff toDiff where

    {-# INLINEABLE fromDiff #-}
    fromDiff :: DiffTime -> TimeOfDay
    fromDiff (DiffTime t) = TimeOfDay
            (fromIntegral h) (fromIntegral m) (DiffTime s) where
        (h, ms) = microQuotRem t (Micro 3600000000)
        (m, s) = microQuotRem ms (Micro 60000000)

    {-# INLINEABLE toDiff #-}
    toDiff :: TimeOfDay -> DiffTime
    toDiff (TimeOfDay h m s) = s
        ^+^ fromIntegral m *^ DiffTime (Micro 60000000)
        ^+^ fromIntegral h *^ DiffTime (Micro 3600000000)

-- | Add some minutes to a 'TimeOfDay'; result comes with a day adjustment.
--
-- ==== Examples
--
-- @
-- > 'addMinutes' 10 ('TimeOfDay' 23 55 0)
--   (1,00:05:00)
-- @
{-# INLINE addMinutes #-}
addMinutes :: Minutes -> TimeOfDay -> (Days, TimeOfDay)
addMinutes dm (TimeOfDay h m s) = (dd, TimeOfDay h' m' s) where
    (dd, h') = divMod (h + dh) 24
    (dh, m') = divMod (m + dm) 60

-- | "Control.Lens.Iso" between 'TimeOfDay' and the fraction of a day.
--
-- ==== Examples
--
-- @
-- > import Data.Ratio
--
-- > 'TimeOfDay' 6 0 0 '^.' 'dayFraction'
--   1 % 4
-- > 'TimeOfDay' 8 0 0 '^.' 'dayFraction'
--   1 % 3
--
-- > 'dayFraction' 'Control.Lens.Review.#' (1 % 4)
--   06:00:00
-- > 'dayFraction' 'Control.Lens.Review.#' (1 % 3)
--   08:00:00
-- @
{-# INLINE dayFraction #-}
dayFraction :: Iso' TimeOfDay Rational
dayFraction = from timeOfDay . iso toRatio fromRatio where

    {-# INLINEABLE toRatio #-}
    toRatio :: DiffTime -> Rational
    toRatio t = toSeconds t / toSeconds posixDayLength

    {-# INLINEABLE fromRatio #-}
    fromRatio :: Rational -> DiffTime
    fromRatio ((*^ posixDayLength) -> NominalDiffTime r) = DiffTime r

------------------------------------------------------------------------
-- * Local Time

-- | Local calendar date and time-of-day.
--
-- This type is appropriate for inputting from and outputting to the
-- outside world.
--
-- To actually perform logic and arithmetic on local date-times, a 'LocalTime'
-- should first be converted to a 'UTCTime' by the 'utcLocalTime' Iso.
--
-- See also 'ZonedTime'.
data LocalTime = LocalTime
    { localDay :: {-# UNPACK #-}!Day
        -- ^ Local calendar date.
    , localTimeOfDay :: {-only 3 words…-} {-# UNPACK #-}!TimeOfDay
        -- ^ Local time-of-day.
    } deriving (INSTANCES_USUAL)

derivingUnbox "LocalTime" [t| LocalTime -> (Day, TimeOfDay) |]
    [| \ LocalTime {..} -> (localDay, localTimeOfDay) |]
    [| \ (localDay, localTimeOfDay) -> LocalTime {..} |]

instance Hashable LocalTime
instance NFData LocalTime

#if SHOW_INTERNAL
deriving instance Show LocalTime
#else
instance Show LocalTime where
    showsPrec p (LocalTime d t) = showsPrec p d . (:) ' ' . showsPrec p t
#endif

instance Bounded LocalTime where
    minBound = minBound ^. utcLocalTime maxBound
    maxBound = maxBound ^. utcLocalTime minBound

instance Random LocalTime where
    randomR = randomIsoR (utcLocalTime utc)
    random = randomR (minBound, maxBound)

instance Arbitrary LocalTime where
    arbitrary = choose (minBound, maxBound)
    shrink lt@LocalTime {..}
        = [ lt {localDay = d} | d <- shrink localDay ]
        ++ [ lt {localTimeOfDay = d} | d <- shrink localTimeOfDay ]

instance CoArbitrary LocalTime where
    coarbitrary (LocalTime d t) = coarbitrary d . coarbitrary t

-- | "Control.Lens.Iso" between 'UTCTime' and 'LocalTime'.
--
-- See also 'zonedTime'.
--
-- ==== Examples
--
-- @
-- > tzJST <- 'getCurrentTimeZone'
--
-- > 'timeZoneName' tzJST
--   \"JST\"
--
-- > 'timeZoneOffsetString' tzJST
--   \"+0900\"
--
-- > tUTC <- 'getCurrentTime'
-- > tUTC
--   2016-04-23 02:00:00.000000 UTC
--
-- > let tJST = tUTC '^.' 'utcLocalTime' tzJST
-- > tJST
--   2016-04-23 11:00:00.000000
--
-- > 'utcLocalTime' tz 'Control.Lens.Review.#' tJST
--   2016-04-23 02:00:00.000000 UTC
-- @
{-# INLINE utcLocalTime #-}
utcLocalTime :: TimeZone -> Iso' UTCTime LocalTime
utcLocalTime TimeZone {..} = utcTime . iso localise globalise where

    {-# INLINEABLE localise #-}
    localise :: UTCView -> LocalTime
    localise (UTCView day dt) = LocalTime (day .+^ dd) tod where
        (dd, tod) = addMinutes timeZoneMinutes (dt ^. timeOfDay)

    {-# INLINEABLE globalise #-}
    globalise :: LocalTime -> UTCView
    globalise (LocalTime day tod) = UTCView (day .+^ dd)
            (timeOfDay # utcToD) where
        (dd, utcToD) = addMinutes (negate timeZoneMinutes) tod

-- | "Control.Lens.Iso" between 'UniversalTime' and 'LocalTime'.
{-# INLINE ut1LocalTime #-}
ut1LocalTime :: Rational -> Iso' UniversalTime LocalTime
ut1LocalTime long = iso localise globalise where
    NominalDiffTime posixDay@(Micro usDay) = posixDayLength

    {-# INLINEABLE localise #-}
    localise :: UniversalTime -> LocalTime
    localise (UniversalRep (NominalDiffTime t)) = LocalTime
            (ModifiedJulianDay $ fromIntegral day)
            (DiffTime dt ^. timeOfDay) where
        (day, dt) = microDivMod (t ^+^ (long / 360) *^ posixDay) posixDay

    {-# INLINEABLE globalise #-}
    globalise :: LocalTime -> UniversalTime
    globalise (LocalTime day tod) = UniversalRep . NominalDiffTime $
            Micro (mjd * usDay) ^+^ dt ^-^ (long / 360) *^ posixDay where
        ModifiedJulianDay (fromIntegral -> mjd) = day
        DiffTime dt = timeOfDay # tod

------------------------------------------------------------------------
-- * Zoned Time

-- | A 'LocalTime' and its 'TimeZone'.
--
-- This type is appropriate for inputting from and outputting to the
-- outside world.
--
-- To actually perform logic and arithmetic on local date-times, a 'ZonedTime'
-- should first be converted to a 'UTCTime' by the 'zonedTime' Iso.
data ZonedTime = ZonedTime
    { zonedTimeToLocalTime :: {-only 4 words…-} {-# UNPACK #-}!LocalTime
        -- ^ Local time.
    , zonedTimeZone :: !TimeZone
        -- ^ Time Zone for the local time.
    } deriving (INSTANCES_USUAL)

instance Hashable ZonedTime
instance NFData ZonedTime where
    rnf ZonedTime {..} = rnf zonedTimeZone

instance Bounded ZonedTime where
    minBound = ZonedTime minBound maxBound
    maxBound = ZonedTime maxBound minBound

instance Random ZonedTime where
    randomR (l, u) g0 = (view zonedTime . (,) tz)
            `first` randomR (l', u') g1 where
        (tz, g1) = random g0 -- ignore TimeZone from l and u
        l' = snd $ zonedTime # l
        u' = snd $ zonedTime # u
    random = randomR (minBound, maxBound)

instance Arbitrary ZonedTime where
    arbitrary = choose (minBound, maxBound)
    shrink zt@ZonedTime {..}
        = [ zt {zonedTimeToLocalTime = lt} | lt <- shrink zonedTimeToLocalTime ]
        ++ [ zt {zonedTimeZone = tz} | tz <- shrink zonedTimeZone ]

instance CoArbitrary ZonedTime where
    coarbitrary (ZonedTime lt tz) = coarbitrary lt . coarbitrary tz

-- | "Control.Lens.Iso" between ('TimeZone', 'UTCTime') and 'ZonedTime'.
--
-- See also 'utcLocalTime'.
--
-- ==== Examples
--
-- @
-- > tLocal <- 'getZonedTime'
--
-- > tLocal
--   2016-04-04 16:00:00.000000 JST
--
-- > 'zonedTime' 'Control.Lens.Review.#' tLocal
--   (JST,2016-04-04 07:00:00.000000 UTC)
--
-- > ('zonedTime' 'Control.Lens.Review.#' tLocal) '^.' 'zonedTime'
--   2016-04-04 16:00:00.000000 JST
-- @
{-# INLINE zonedTime #-}
zonedTime :: Iso' (TimeZone, UTCTime) ZonedTime
zonedTime = iso toZoned fromZoned where

    {-# INLINE toZoned #-}
    toZoned :: (TimeZone, UTCTime) -> ZonedTime
    toZoned (tz, time) = ZonedTime (time ^. utcLocalTime tz) tz

    {-# INLINE fromZoned #-}
    fromZoned :: ZonedTime -> (TimeZone, UTCTime)
    fromZoned (ZonedTime lt tz) = (tz, utcLocalTime tz # lt)

#if SHOW_INTERNAL
deriving instance Show ZonedTime
instance Show UTCTime where
    showsPrec p = showsPrec p . view utcTime
#else
instance Show ZonedTime where
    showsPrec p (ZonedTime lt tz) = showsPrec p lt . (:) ' ' . showsPrec p tz
instance Show UTCTime where
    showsPrec p = showsPrec p . view zonedTime . (,) utc
#endif

-- | Get the current local system date and time, as well as the current local
-- system Time Zone.
--
-- See also 'getCurrentTime', 'Data.Thyme.Clock.POSIX.getPOSIXTime'.
--
-- ==== Examples
--
-- @
-- > getZonedTime
--   2016-04-23 11:57:22.516064 JST
-- @
{-# INLINE getZonedTime #-}
getZonedTime :: IO ZonedTime
getZonedTime = utcToLocalZonedTime =<< getCurrentTime

-- | Convert a 'UTCTime' to a 'ZonedTime' according to the local system
-- Time Zone returned by 'getTimeZone'.
--
-- See also 'zonedTime'.
{-# INLINEABLE utcToLocalZonedTime #-}
utcToLocalZonedTime :: UTCTime -> IO ZonedTime
utcToLocalZonedTime time = do
    tz <- getTimeZone time
    return $ (tz, time) ^. zonedTime

-- * Lenses

LENS(TimeZone,timeZoneMinutes,Minutes)
LENS(TimeZone,timeZoneSummerOnly,Bool)
LENS(TimeZone,timeZoneName,String)

LENS(TimeOfDay,todHour,Hour)
LENS(TimeOfDay,todMin,Minute)
LENS(TimeOfDay,todSec,DiffTime)

LENS(LocalTime,localDay,Day)
LENS(LocalTime,localTimeOfDay,TimeOfDay)

LENS(ZonedTime,zonedTimeToLocalTime,LocalTime)
LENS(ZonedTime,zonedTimeZone,TimeZone)

