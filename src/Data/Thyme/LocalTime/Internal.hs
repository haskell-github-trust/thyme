{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

#include "thyme.h"

module Data.Thyme.LocalTime.Internal where

import Prelude hiding ((.))
import Control.Applicative
import Control.Category hiding (id)
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Data
import Data.Thyme.Internal.Micro
import Data.Thyme.Calendar.Internal
import Data.Thyme.Clock.Internal
#if !SHOW_INTERNAL
import Data.Thyme.Format.Internal
#endif
import Data.Thyme.LocalTime.TimeZone
import Data.VectorSpace
import GHC.Generics (Generic)
import System.Random
import Test.QuickCheck

------------------------------------------------------------------------
-- * Time of day

type Hour = Int
type Minute = Int
data TimeOfDay = TimeOfDay
    { todHour :: {-# UNPACK #-}!Hour
    , todMin :: {-# UNPACK #-}!Minute
    , todSec :: {-# UNPACK #-}!DiffTime
    } deriving (INSTANCES_USUAL)

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
    random = over _1 (^. timeOfDay) . random

instance Arbitrary TimeOfDay where
    arbitrary = do
        h <- choose (0, 23)
        m <- choose (0, 59)
        let DiffTime ml = minuteLength h m
        TimeOfDay h m . DiffTime <$> choose (zeroV, pred ml)

{-# INLINE minuteLength #-}
minuteLength :: Hour -> Minute -> DiffTime
minuteLength h m = fromSeconds' $ if h == 23 && m == 59 then 61 else 60

-- | Hour zero
midnight :: TimeOfDay
midnight = TimeOfDay 0 0 zeroV

-- | Hour twelve
midday :: TimeOfDay
midday = TimeOfDay 12 0 zeroV

{-# INLINE makeTimeOfDayValid #-}
makeTimeOfDayValid :: Hour -> Minute -> DiffTime -> Maybe TimeOfDay
makeTimeOfDayValid h m s = TimeOfDay h m s
    <$ guard (0 <= h && h <= 23 && 0 <= m && m <= 59)
    <* guard (zeroV <= s && s < minuteLength h m)

{-# INLINE timeOfDay #-}
timeOfDay :: Iso' DiffTime TimeOfDay
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

-- | Add some minutes to a 'TimeOfDay'; result comes with a day adjustment.
{-# INLINE addMinutes #-}
addMinutes :: Minutes -> TimeOfDay -> (Days, TimeOfDay)
addMinutes dm (TimeOfDay h m s) = (dd, TimeOfDay h' m' s) where
    (dd, h') = divMod (h + dh) 24
    (dh, m') = divMod (m + dm) 60

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

data LocalTime = LocalTime
    { localDay :: {-# UNPACK #-}!Day
    , localTimeOfDay :: {-only 3 words…-} {-# UNPACK #-}!TimeOfDay
    } deriving (INSTANCES_USUAL)

instance NFData LocalTime

#if SHOW_INTERNAL
deriving instance Show LocalTime
#else
instance Show LocalTime where
    showsPrec p (LocalTime d t) = showsPrec p d . (:) ' ' . showsPrec p t
#endif

{-# INLINE utcLocalTime #-}
utcLocalTime :: TimeZone -> Iso' UTCTime LocalTime
utcLocalTime TimeZone {..} = utcTime . iso localise globalise where

    {-# INLINEABLE localise #-}
    localise :: UTCView -> LocalTime
    localise (UTCTime day dt) = LocalTime (day .+^ dd) tod where
        (dd, tod) = addMinutes timeZoneMinutes (dt ^. timeOfDay)

    {-# INLINEABLE globalise #-}
    globalise :: LocalTime -> UTCView
    globalise (LocalTime day tod) = UTCTime (day .+^ dd)
            (timeOfDay # utcToD) where
        (dd, utcToD) = addMinutes (negate timeZoneMinutes) tod

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

data ZonedTime = ZonedTime
    { zonedTimeToLocalTime :: {-only 4 words…-} {-# UNPACK #-}!LocalTime
    , zonedTimeZone :: !TimeZone
    } deriving (INSTANCES_USUAL)

instance NFData ZonedTime where
    rnf ZonedTime {..} = rnf zonedTimeZone

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

