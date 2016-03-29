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

module Data.Thyme.LocalTime where

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

type Minutes = Int
type Hours = Int

------------------------------------------------------------------------
-- * Time zones

data TimeZone = TimeZone
    { timeZoneMinutes :: {-# UNPACK #-}!Minutes
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
-- \"+0400\" (like %z in 'formatTime')
{-# INLINEABLE timeZoneOffsetString #-}
timeZoneOffsetString :: TimeZone -> String
timeZoneOffsetString TimeZone {..} = sign : (shows02 h . shows02 m) "" where
    (h, m) = divMod offset 60
    (sign, offset) = if timeZoneMinutes < 0
        then ('-', negate timeZoneMinutes) else ('+', timeZoneMinutes)

-- | Create a nameless non-summer timezone for this number of minutes
minutesToTimeZone :: Minutes -> TimeZone
minutesToTimeZone m = TimeZone m False ""

-- | Create a nameless non-summer timezone for this number of hours
hoursToTimeZone :: Hours -> TimeZone
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

------------------------------------------------------------------------
-- * Time of day

type Hour = Int
type Minute = Int
data TimeOfDay = TimeOfDay
    { todHour :: {-# UNPACK #-}!Hour
    , todMin :: {-# UNPACK #-}!Minute
    , todSec :: {-# UNPACK #-}!DiffTime
    } deriving (INSTANCES_USUAL)

derivingUnbox "TimeOfDay" [t| TimeOfDay -> Int64 |]
    [| \ TimeOfDay {..} -> fromIntegral (todHour .|. shiftL todMin 8)
        .|. shiftL (todSec ^. microseconds) 16 |]
    [| \ n -> TimeOfDay (fromIntegral $ n .&. 0xff)
        (fromIntegral $ shiftR n 8 .&. 0xff) (microseconds # shiftR n 16) |]

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
        (h, ms) = microQuotRem t (Micro 3600000000)
        (m, s) = microQuotRem ms (Micro 60000000)

    {-# INLINEABLE toDiff #-}
    toDiff :: TimeOfDay -> DiffTime
    toDiff (TimeOfDay h m s) = s
        ^+^ fromIntegral m *^ DiffTime (Micro 60000000)
        ^+^ fromIntegral h *^ DiffTime (Micro 3600000000)

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

derivingUnbox "LocalTime" [t| LocalTime -> (Day, TimeOfDay) |]
    [| \ LocalTime {..} -> (localDay, localTimeOfDay) |]
    [| \ (localDay, localTimeOfDay) -> LocalTime {..} |]

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

{-# INLINE getZonedTime #-}
getZonedTime :: IO ZonedTime
getZonedTime = utcToLocalZonedTime =<< getCurrentTime

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

