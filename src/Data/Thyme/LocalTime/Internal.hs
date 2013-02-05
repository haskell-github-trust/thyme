{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- #hide
module Data.Thyme.LocalTime.Internal where

import Prelude hiding ((.))
import Control.Applicative
import Control.Category hiding (id)
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Data
import Data.Micro
import Data.Thyme.Calendar
import Data.Thyme.Clock.Internal
#if !SHOW_INTERNAL
import Data.Thyme.Format.Internal
#endif
import Data.Thyme.LocalTime.TimeZone
import Data.VectorSpace

------------------------------------------------------------------------
-- * Time of day

type Hour = Int
type Minute = Int
data TimeOfDay = TimeOfDay
    { todHour :: {-# UNPACK #-}!Hour
    , todMin :: {-# UNPACK #-}!Minute
    , todSec :: {-# UNPACK #-}!DiffTime
    } deriving (Eq, Ord, Data, Typeable)

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

-- | Hour zero
midnight :: TimeOfDay
midnight = TimeOfDay 0 0 zeroV

-- | Hour twelve
midday :: TimeOfDay
midday = TimeOfDay 12 0 zeroV

{-# INLINE makeTimeOfDayValid #-}
makeTimeOfDayValid :: Hour -> Minute -> DiffTime -> Maybe TimeOfDay
makeTimeOfDayValid h m s@(DiffTime u) = TimeOfDay h m s
    <$ guard (0 <= h && h <= 23 && 0 <= m && m <= 59)
    <* guard (Micro 0 <= u && u < Micro 61000000)

{-# INLINE timeOfDay #-}
timeOfDay :: Simple Iso DiffTime TimeOfDay
timeOfDay = iso fromDiff toDiff where

    {-# INLINEABLE fromDiff #-}
    fromDiff :: DiffTime -> TimeOfDay
    fromDiff (DiffTime t) = TimeOfDay
            (fromIntegral h) (fromIntegral m) (DiffTime s) where
        (h, ms) = microQuotRem t (toMicro 3600)
        (m, s) = microQuotRem ms (toMicro 60)

    {-# INLINEABLE toDiff #-}
    toDiff :: TimeOfDay -> DiffTime
    toDiff (TimeOfDay h m s) = s
        ^+^ fromIntegral m *^ DiffTime (toMicro 60)
        ^+^ fromIntegral h *^ DiffTime (toMicro 3600)

type Minutes = Int
type Days = Int

-- | Add some minutes to a 'TimeOfDay'; result comes with a day adjustment.
{-# INLINE addMinutes #-}
addMinutes :: Minutes -> TimeOfDay -> (Days, TimeOfDay)
addMinutes dm (TimeOfDay h m s) = (dd, TimeOfDay h' m' s) where
    (dd, h') = divMod (h + dh) 24
    (dh, m') = divMod (m + dm) 60

{-# INLINE timeOfDayFraction #-}
timeOfDayFraction :: Simple Iso Rational TimeOfDay
timeOfDayFraction = iso fromRatio toRatio . timeOfDay where
    NominalDiffTime posixDay = posixDayLength

    fromRatio :: Rational -> DiffTime
    fromRatio r = DiffTime (r *^ posixDay)

    toRatio :: DiffTime -> Rational
    toRatio (DiffTime t) = t ^/^ posixDay

------------------------------------------------------------------------
-- * Local Time

data LocalTime = LocalTime
    { localDay :: {-# UNPACK #-}!Day
    , localTimeOfDay :: {-only 3 words…-} {-# UNPACK #-}!TimeOfDay
    } deriving (Eq, Ord, Data, Typeable)

#if SHOW_INTERNAL
deriving instance Show LocalTime
#else
instance Show LocalTime where
    showsPrec p (LocalTime d t) = showsPrec p d . (:) ' ' . showsPrec p t
#endif

{-# INLINE utcLocalTime #-}
utcLocalTime :: TimeZone -> Simple Iso UTCTime LocalTime
utcLocalTime TimeZone {..} = utcTime . iso localise globalise where

    {-# INLINEABLE localise #-}
    localise :: UTCView -> LocalTime
    localise (UTCTime day dt) = LocalTime (day .+^ dd) tod where
        (dd, tod) = addMinutes timeZoneMinutes (view timeOfDay dt)

    {-# INLINEABLE globalise #-}
    globalise :: LocalTime -> UTCView
    globalise (LocalTime day tod) = UTCTime (day .+^ dd)
            (review timeOfDay utcToD) where
        (dd, utcToD) = addMinutes (negate timeZoneMinutes) tod

{-# INLINE ut1LocalTime #-}
ut1LocalTime :: Rational -> Simple Iso UniversalTime LocalTime
ut1LocalTime long = iso localise globalise where
    NominalDiffTime posixDay@(Micro usDay) = posixDayLength

    {-# INLINEABLE localise #-}
    localise :: UniversalTime -> LocalTime
    localise (UniversalRep (NominalDiffTime t)) = LocalTime
            (ModifiedJulianDay day) (view timeOfDay (DiffTime dt)) where
        (day, dt) = microDivMod (t ^+^ (long / 360) *^ posixDay) posixDay

    {-# INLINEABLE globalise #-}
    globalise :: LocalTime -> UniversalTime
    globalise (LocalTime day tod) = UniversalRep . NominalDiffTime $
            Micro (mjd * usDay) ^+^ dt ^-^ (long / 360) *^ posixDay where
        ModifiedJulianDay mjd = day
        DiffTime dt = review timeOfDay tod

------------------------------------------------------------------------
-- * Zoned Time

data ZonedTime = ZonedTime
    { zonedTimeToLocalTime :: {-only 4 words…-} {-# UNPACK #-}!LocalTime
    , zonedTimeZone :: !TimeZone
    } deriving (Eq, Ord, Data, Typeable)

{-# INLINE zonedTime #-}
zonedTime :: Simple Iso (TimeZone, UTCTime) ZonedTime
zonedTime = iso toZoned fromZoned where

    {-# INLINE toZoned #-}
    toZoned :: (TimeZone, UTCTime) -> ZonedTime
    toZoned (tz, time) = ZonedTime (view (utcLocalTime tz) time) tz

    {-# INLINE fromZoned #-}
    fromZoned :: ZonedTime -> (TimeZone, UTCTime)
    fromZoned (ZonedTime lt tz) = (tz, review (utcLocalTime tz) lt)

#if SHOW_INTERNAL
deriving instance Show ZonedTime
#else
instance Show ZonedTime where
    showsPrec p (ZonedTime lt tz) = showsPrec p lt . (:) ' ' . showsPrec p tz
#endif

