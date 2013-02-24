{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides just the compatibility wrappers for the things
-- that @thyme@ does differently from @time@. No 'RealFrac' instances for
-- 'DiffTime' nor 'NominalDiffTime', nor other riffraff.

module Data.Thyme.Time.Core
    ( module Data.Thyme
    , module Data.Thyme.Time.Core
    ) where

import Prelude
import Control.Lens
import Data.AffineSpace
import Data.Int
import Data.Micro
import Data.Ratio
import Data.Thyme
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.MonthDay
import Data.Thyme.Calendar.WeekDate
import Data.Thyme.Clock.Internal
import Data.Thyme.Clock.POSIX
import Data.Thyme.Clock.TAI
import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.TAI as T
import qualified Data.Time.LocalTime as T

------------------------------------------------------------------------
-- * Type conversion

class Thyme a b | b -> a where
    thyme :: Iso' a b

instance Thyme T.Day Day where
    {-# INLINE thyme #-}
    thyme = iso
        (ModifiedJulianDay . fromInteger . T.toModifiedJulianDay)
        (T.ModifiedJulianDay . fromIntegral . toModifiedJulianDay)

instance Thyme T.UniversalTime UniversalTime where
    {-# INLINE thyme #-}
    thyme = iso T.getModJulianDate T.ModJulianDate . from modJulianDate

instance Thyme T.DiffTime DiffTime where
    {-# INLINE thyme #-}
    thyme = iso (round . (*) 1000000)
        (T.picosecondsToDiffTime . (*) 1000000 . toInteger) . microDiffTime

instance Thyme T.UTCTime UTCView where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.UTCTime d t) -> UTCTime (view thyme d) (view thyme t))
        (\ (UTCTime d t) -> T.UTCTime (review thyme d) (review thyme t))

instance Thyme T.UTCTime UTCTime where
    {-# INLINE thyme #-}
    thyme = thyme . from utcTime

instance Thyme T.NominalDiffTime NominalDiffTime where
    {-# INLINE thyme #-}
    thyme = iso (round . (*) 1000000) -- no picosecondsToNominalDiffTime D:
        (fromRational . (% 1000000) . toInteger) . microNominalDiffTime

instance Thyme T.AbsoluteTime AbsoluteTime where
    {-# INLINE thyme #-}
    thyme = iso (`T.diffAbsoluteTime` T.taiEpoch)
            (`T.addAbsoluteTime` T.taiEpoch)
        . thyme . iso (taiEpoch .+^) (.-. taiEpoch)

instance Thyme T.TimeZone TimeZone where
    {-# INLINE thyme #-}
    thyme = id

instance Thyme T.TimeOfDay TimeOfDay where
    {-# INLINE thyme #-}
    thyme = iso ( \ (T.TimeOfDay h m s) -> TimeOfDay h m
            . view microDiffTime . round $ s * 1000000 )
        ( \ (TimeOfDay h m s) -> T.TimeOfDay h m . fromRational
            . (% 1000000) . toInteger $ review microDiffTime s )

instance Thyme T.LocalTime LocalTime where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.LocalTime d t) -> LocalTime (view thyme d) (view thyme t))
        (\ (LocalTime d t) -> T.LocalTime (review thyme d) (review thyme t))

instance Thyme T.ZonedTime ZonedTime where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.ZonedTime t z) -> ZonedTime (view thyme t) (view thyme z))
        (\ (ZonedTime t z) -> T.ZonedTime (review thyme t) (review thyme z))

{-# INLINE toThyme #-}
toThyme :: (Thyme a b) => a -> b
toThyme = view thyme

{-# INLINE fromThyme #-}
fromThyme :: (Thyme a b) => b -> a
fromThyme = review thyme

------------------------------------------------------------------------
-- * @Data.Time.Calendar@

{-# INLINE addDays #-}
addDays :: Days -> Day -> Day
addDays = flip (.+^)

{-# INLINE diffDays #-}
diffDays :: Day -> Day -> Days
diffDays = (.-.)

{-# INLINE toGregorian #-}
toGregorian :: Day -> (Year, Month, DayOfMonth)
toGregorian (view gregorian -> YearMonthDay y m d) = (y, m, d)

{-# INLINE fromGregorian #-}
fromGregorian :: Year -> Month -> DayOfMonth -> Day
fromGregorian y m d = review gregorian (YearMonthDay y m d)

{-# INLINE fromGregorianValid #-}
fromGregorianValid :: Year -> Month -> DayOfMonth -> Maybe Day
fromGregorianValid y m d = gregorianValid (YearMonthDay y m d)

{-# INLINE addGregorianMonthsClip #-}
addGregorianMonthsClip :: Months -> Day -> Day
addGregorianMonthsClip n = review gregorian
    . gregorianMonthsClip n . view gregorian

{-# INLINE addGregorianMonthsRollover #-}
addGregorianMonthsRollover :: Months -> Day -> Day
addGregorianMonthsRollover n = review gregorian
    . gregorianMonthsRollover n . view gregorian

{-# INLINE addGregorianYearsClip #-}
addGregorianYearsClip :: Years -> Day -> Day
addGregorianYearsClip n = review gregorian
    . gregorianYearsClip n . view gregorian

{-# INLINE addGregorianYearsRollover #-}
addGregorianYearsRollover :: Years -> Day -> Day
addGregorianYearsRollover n = review gregorian
    . gregorianYearsRollover n . view gregorian

------------------------------------------------------------------------
-- * @Data.Time.Calendar.MonthDay@

{-# INLINE dayOfYearToMonthAndDay #-}
dayOfYearToMonthAndDay :: Bool -> DayOfYear -> (Month, DayOfMonth)
dayOfYearToMonthAndDay leap (view (monthDay leap) -> MonthDay m d) = (m, d)

{-# INLINE monthAndDayToDayOfYear #-}
monthAndDayToDayOfYear :: Bool -> Month -> DayOfMonth -> DayOfYear
monthAndDayToDayOfYear leap m d = review (monthDay leap) (MonthDay m d)

{-# INLINE monthAndDayToDayOfYearValid #-}
monthAndDayToDayOfYearValid :: Bool -> Month -> DayOfMonth -> Maybe DayOfYear
monthAndDayToDayOfYearValid leap m d = monthDayValid leap (MonthDay m d)

------------------------------------------------------------------------
-- * @Data.Time.Calendar.OrdinalDate@

{-# INLINE toOrdinalDate #-}
toOrdinalDate :: Day -> (Year, DayOfYear)
toOrdinalDate (view ordinalDate -> OrdinalDate y d) = (y, d)

{-# INLINE fromOrdinalDate #-}
fromOrdinalDate :: Year -> DayOfYear -> Day
fromOrdinalDate y d = review ordinalDate (OrdinalDate y d)

{-# INLINE fromOrdinalDateValid #-}
fromOrdinalDateValid :: Year -> DayOfYear -> Maybe Day
fromOrdinalDateValid y d = ordinalDateValid (OrdinalDate y d)

{-# INLINE sundayStartWeek #-}
sundayStartWeek :: Day -> (Year, WeekOfYear, DayOfWeek)
sundayStartWeek (view sundayWeek -> SundayWeek y w d) = (y, w, d)

{-# INLINE fromSundayStartWeek #-}
fromSundayStartWeek :: Year -> WeekOfYear -> DayOfWeek -> Day
fromSundayStartWeek y w d = review sundayWeek (SundayWeek y w d)

{-# INLINE fromSundayStartWeekValid #-}
fromSundayStartWeekValid :: Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromSundayStartWeekValid y w d = sundayWeekValid (SundayWeek y w d)

{-# INLINE mondayStartWeek #-}
mondayStartWeek :: Day -> (Year, WeekOfYear, DayOfWeek)
mondayStartWeek (view mondayWeek -> MondayWeek y w d) = (y, w, d)

{-# INLINE fromMondayStartWeek #-}
fromMondayStartWeek :: Year -> WeekOfYear -> DayOfWeek -> Day
fromMondayStartWeek y w d = review mondayWeek (MondayWeek y w d)

{-# INLINE fromMondayStartWeekValid #-}
fromMondayStartWeekValid :: Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromMondayStartWeekValid y w d = mondayWeekValid (MondayWeek y w d)

------------------------------------------------------------------------
-- * @Data.Time.Calendar.WeekDate@

{-# INLINE toWeekDate #-}
toWeekDate :: Day -> (Year, WeekOfYear, DayOfWeek)
toWeekDate (view weekDate -> WeekDate y w d) = (y, w, d)

{-# INLINE fromWeekDate #-}
fromWeekDate :: Year -> WeekOfYear -> DayOfWeek -> Day
fromWeekDate y w d = review weekDate (WeekDate y w d)

{-# INLINE fromWeekDateValid #-}
fromWeekDateValid :: Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromWeekDateValid y w d = weekDateValid (WeekDate y w d)

------------------------------------------------------------------------
-- * @Data.Time.Clock@

{-# INLINE getModJulianDate #-}
getModJulianDate :: UniversalTime -> Rational
getModJulianDate = view modJulianDate

-- | Replacement for 'T.ModJulianDate'.
{-# INLINE mkModJulianDate #-}
mkModJulianDate :: Rational -> UniversalTime
mkModJulianDate = review modJulianDate

{-# INLINE secondsToDiffTime #-}
secondsToDiffTime :: Int64 -> DiffTime
secondsToDiffTime a = DiffTime (Micro $ a * 1000000)

{-# INLINE picosecondsToDiffTime #-}
picosecondsToDiffTime :: Int64 -> DiffTime
picosecondsToDiffTime a = DiffTime (Micro $ div (a + 500000) 1000000)

{-# INLINE mkUTCTime #-}
mkUTCTime :: Day -> DiffTime -> UTCTime
mkUTCTime d t = review utcTime (UTCTime d t)

{-# INLINE unUTCTime #-}
unUTCTime :: UTCTime -> UTCView
unUTCTime = view utcTime

{-# INLINE addUTCTime #-}
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime = flip (.+^)

{-# INLINE diffUTCTime #-}
diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime = (.-.)

------------------------------------------------------------------------
-- * @Data.Time.Clock.POSIX@

{-# INLINE posixSecondsToUTCTime #-}
posixSecondsToUTCTime :: POSIXTime -> UTCTime
posixSecondsToUTCTime = review posixTime

{-# INLINE utcTimeToPOSIXSeconds #-}
utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
utcTimeToPOSIXSeconds = view posixTime

------------------------------------------------------------------------
-- * @Data.Time.Clock.TAI@

{-# INLINE addAbsoluteTime #-}
addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime = flip (.+^)

{-# INLINE diffAbsoluteTime #-}
diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime = (.-.)

{-# INLINE utcToTAITime #-}
utcToTAITime :: LeapSecondTable -> UTCTime -> AbsoluteTime
utcToTAITime = view . absoluteTime

{-# INLINE taiToUTCTime #-}
taiToUTCTime :: LeapSecondTable -> AbsoluteTime -> UTCTime
taiToUTCTime = review . absoluteTime

------------------------------------------------------------------------
-- * @Data.Time.LocalTime@

{-# INLINE utcToLocalTimeOfDay #-}
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Days, TimeOfDay)
utcToLocalTimeOfDay = addMinutes . timeZoneMinutes

{-# INLINE localToUTCTimeOfDay #-}
localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Days, TimeOfDay)
localToUTCTimeOfDay = addMinutes . negate . timeZoneMinutes

{-# INLINE timeToTimeOfDay #-}
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay = view timeOfDay

{-# INLINE timeOfDayToTime #-}
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime = review timeOfDay

{-# INLINE dayFractionToTimeOfDay #-}
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay = review dayFraction

{-# INLINE timeOfDayToDayFraction #-}
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction = view dayFraction

{-# INLINE utcToLocalTime #-}
utcToLocalTime :: TimeZone -> UTCTime -> LocalTime
utcToLocalTime = view . utcLocalTime

{-# INLINE localTimeToUTC #-}
localTimeToUTC :: TimeZone -> LocalTime -> UTCTime
localTimeToUTC = review . utcLocalTime

{-# INLINE ut1ToLocalTime #-}
ut1ToLocalTime :: Rational -> UniversalTime -> LocalTime
ut1ToLocalTime = view . ut1LocalTime

{-# INLINE localTimeToUT1 #-}
localTimeToUT1 :: Rational -> LocalTime -> UniversalTime
localTimeToUT1 = review . ut1LocalTime

{-# INLINE utcToZonedTime #-}
utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime
utcToZonedTime z t = view zonedTime (z, t)

{-# INLINE zonedTimeToUTC #-}
zonedTimeToUTC :: ZonedTime -> UTCTime
zonedTimeToUTC = snd . review zonedTime

