{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

#if HLINT
#include "cabal_macros.h"
#endif

-- | This module provides just the compatibility wrappers for the things
-- that @thyme@ does differently from @time@. No 'RealFrac' instances for
-- 'DiffTime' nor 'NominalDiffTime', nor other riffraff.
--
-- === References to @time@
--
-- * "Data.Time.Calendar"
-- * "Data.Time.Calendar.MonthDay"
-- * "Data.Time.Calendar.OrdinalDate"
-- * "Data.Time.Calendar.WeekDate"
-- * "Data.Time.Clock"
-- * "Data.Time.Clock.POSIX"
-- * "Data.Time.Clock.TAI"
-- * "Data.Time.LocalTime"

module Data.Thyme.Time.Core
    ( module Data.Thyme
    , module Data.Thyme.Time.Core
    ) where

import Prelude
import Control.Lens
import Data.AffineSpace
import Data.Fixed
import Data.Int
import Data.Thyme.Internal.Micro
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
import Unsafe.TrueName

------------------------------------------------------------------------
-- * Type conversion

-- | Typeclass for "Data.Thyme" types for which an "Control.Lens.Iso" exists
-- to an equivalent type in the "Data.Time" library.
class Thyme a b | b -> a where
    -- | "Control.Lens.Iso" between "Data.Thyme" types and "Data.Time" types.
    --
    -- ==== Examples
    --
    -- @
    -- > import qualified Data.Time
    --
    -- > :t 'thyme' 'Control.Lens.Review.#' ('fromSeconds'' 10 :: 'DiffTime')
    --   'thyme' 'Control.Lens.Review.#' ('fromSeconds'' 10 :: 'DiffTime')
    --     :: Data.Time.DiffTime
    --
    -- > :t Data.Time.Clock.secondsToDiffTime 10 '^.' 'thyme' :: 'DiffTime'
    --   Data.Time.Clock.secondsToDiffTime 10 '^.' 'thyme' :: 'DiffTime'
    --     :: 'DiffTime'
    -- @
    thyme :: Iso' a b

instance Thyme T.Day Day where
    {-# INLINE thyme #-}
    thyme = iso
        (ModifiedJulianDay . fromInteger . T.toModifiedJulianDay)
        (T.ModifiedJulianDay . toInteger . toModifiedJulianDay)

instance Thyme T.UniversalTime UniversalTime where
    {-# INLINE thyme #-}
    thyme = iso T.getModJulianDate T.ModJulianDate . from modJulianDate

instance Thyme T.DiffTime DiffTime where
    {-# INLINE thyme #-}
    thyme = dt . fixed . from picoseconds where
        dt = iso (\ [truename| ''T.DiffTime MkDiffTime | ps |] -> ps )
            [truename| ''T.DiffTime MkDiffTime |]
#if MIN_VERSION_base(4,7,0)
        fixed = iso (\ (MkFixed n) -> n ) MkFixed
#else
        fixed = iso (\ [truename| ''Fixed MkFixed | n |] -> n )
            [truename| ''Fixed MkFixed |]
#endif

instance Thyme T.NominalDiffTime NominalDiffTime where
    {-# INLINE thyme #-}
    thyme = ndt . fixed . from picoseconds where
        ndt = iso (\ [truename| ''T.NominalDiffTime MkNominalDiffTime | ps |] -> ps )
            [truename| ''T.NominalDiffTime MkNominalDiffTime |]
#if MIN_VERSION_base(4,7,0)
        fixed = iso (\ (MkFixed n) -> n ) MkFixed
#else
        fixed = iso (\ [truename| ''Fixed MkFixed | n |] -> n )
            [truename| ''Fixed MkFixed |]
#endif

instance Thyme T.UTCTime UTCView where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.UTCTime d t) -> UTCView (d ^. thyme) (t ^. thyme))
        (\ (UTCView d t) -> T.UTCTime (thyme # d) (thyme # t))

instance Thyme T.UTCTime UTCTime where
    {-# INLINE thyme #-}
    thyme = thyme . from utcTime

instance Thyme T.AbsoluteTime AbsoluteTime where
    {-# INLINE thyme #-}
    thyme = iso (`T.diffAbsoluteTime` T.taiEpoch)
            (`T.addAbsoluteTime` T.taiEpoch)
        . thyme . iso (taiEpoch .+^) (.-. taiEpoch)

instance Thyme T.TimeZone TimeZone where
    {-# INLINE thyme #-}
    thyme = iso (\ T.TimeZone {..} -> TimeZone {..})
        (\ TimeZone {..} -> T.TimeZone {..})

instance Thyme T.TimeOfDay TimeOfDay where
    {-# INLINE thyme #-}
    thyme = iso ( \ (T.TimeOfDay h m s) -> TimeOfDay h m $
            microseconds # round (s * 1000000) )
        ( \ (TimeOfDay h m s) -> T.TimeOfDay h m . fromRational $
            toInteger (s ^. microseconds) % 1000000 )

instance Thyme T.LocalTime LocalTime where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.LocalTime d t) -> LocalTime (d ^. thyme) (t ^. thyme))
        (\ (LocalTime d t) -> T.LocalTime (thyme # d) (thyme # t))

instance Thyme T.ZonedTime ZonedTime where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.ZonedTime t z) -> ZonedTime (t ^. thyme) (z ^. thyme))
        (\ (ZonedTime t z) -> T.ZonedTime (thyme # t) (thyme # z))

-- | Convert from "Data.Time" type to "Data.Thyme" type.
--
-- See also 'thyme'.
{-# INLINE toThyme #-}
toThyme :: (Thyme a b) => a -> b
toThyme = view thyme

-- | Convert from "Data.Thyme" type to "Data.Time" type.
--
-- See also 'thyme'.
{-# INLINE fromThyme #-}
fromThyme :: (Thyme a b) => b -> a
fromThyme = review thyme

------------------------------------------------------------------------
-- * @Data.Time.Calendar@

-- | Add some 'Days' to a calendar 'Day' to get a new 'Day'.
--
-- See also 'Day' 'Data.AffineSpace.AffineSpace' instance.
{-# INLINE addDays #-}
addDays :: Days -> Day -> Day
addDays = flip (.+^)

-- | Subtract two calendar 'Day's for the difference in 'Days'.
--
-- See also 'Day' 'Data.AffineSpace.AffineSpace' instance.
{-# INLINE diffDays #-}
diffDays :: Day -> Day -> Days
diffDays = (.-.)

-- | Get the Gregorian calendar date from a 'Day'.
--
-- See also 'gregorian'.
{-# INLINE toGregorian #-}
toGregorian :: Day -> (Year, Month, DayOfMonth)
toGregorian (view gregorian -> YearMonthDay y m d) = (y, m, d)

-- | Construct a 'Day' from a Gregorian calendar date. Does not validate the
-- date.
--
-- See also 'gregorian'.
{-# INLINE fromGregorian #-}
fromGregorian :: Year -> Month -> DayOfMonth -> Day
fromGregorian y m d = gregorian # YearMonthDay y m d

-- | Construct a 'Day' from a Gregorian calendar date. Returns Nothing if the
-- date is invalid.
--
-- See also 'gregorian'.
{-# INLINE fromGregorianValid #-}
fromGregorianValid :: Year -> Month -> DayOfMonth -> Maybe Day
fromGregorianValid y m d = gregorianValid (YearMonthDay y m d)

-- | Add months, with days past the last day of the month clipped to the last
-- day, according to the
-- <https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar proleptic Gregorian calendar>.
--
-- See also 'gregorianMonthsClip'.
{-# INLINE addGregorianMonthsClip #-}
addGregorianMonthsClip :: Months -> Day -> Day
addGregorianMonthsClip n = review gregorian
    . gregorianMonthsClip n . view gregorian

-- | Add months, with days past the last day of the month rolling over to the
-- next month, according to the
-- <https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar proleptic Gregorian calendar>.
--
-- See also 'gregorianMonthsRollover'.
{-# INLINE addGregorianMonthsRollover #-}
addGregorianMonthsRollover :: Months -> Day -> Day
addGregorianMonthsRollover n = review gregorian
    . gregorianMonthsRollover n . view gregorian

-- | Add years, matching month and day, with /Feb 29th/ clipped to /Feb 28th/ if
-- necessary.
--
-- See also 'gregorianYearsClip'.
{-# INLINE addGregorianYearsClip #-}
addGregorianYearsClip :: Years -> Day -> Day
addGregorianYearsClip n = review gregorian
    . gregorianYearsClip n . view gregorian

-- | Add years, matching month and day, with /Feb 29th/ rolled over to /Mar 1st/ if
-- necessary, according to the
-- <https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar proleptic Gregorian calendar>.
--
-- See also 'gregorianYearsRollover'.
{-# INLINE addGregorianYearsRollover #-}
addGregorianYearsRollover :: Years -> Day -> Day
addGregorianYearsRollover n = review gregorian
    . gregorianYearsRollover n . view gregorian

------------------------------------------------------------------------
-- * @Data.Time.Calendar.MonthDay@

-- | Predicated on whether or not the year is a leap year, convert an ordinal
-- 'DayOfYear' to a ('Month','DayOfMonth')
--
-- See also 'Data.Thyme.Calendar.MonthDay.monthDay'.
{-# INLINE dayOfYearToMonthAndDay #-}
dayOfYearToMonthAndDay
    :: Bool
        -- ^ 'Data.Thyme.Calendar.isLeapYear'?
    -> DayOfYear
    -> (Month, DayOfMonth)
dayOfYearToMonthAndDay leap (view (monthDay leap) -> MonthDay m d) = (m, d)

-- | Predicated on whether or not the year is a leap year, convert
-- a 'Month' and 'DayOfMonth' to an ordinal 'DayOfYear'.
--
-- See also 'Data.Thyme.Calendar.MonthDay.monthDay',
-- 'Data.Thyme.Calendar.MonthDay.monthDayValid',
-- 'monthAndDayToDayOfYearValid'.
{-# INLINE monthAndDayToDayOfYear #-}
monthAndDayToDayOfYear
    :: Bool
        -- ^ 'Data.Thyme.Calendar.isLeapYear'?
    -> Month
    -> DayOfMonth
    -> DayOfYear
monthAndDayToDayOfYear leap m d = monthDay leap # MonthDay m d

-- | Predicated on whether or not the year is a leap year, convert
-- a 'Month' and 'DayOfMonth' to an ordinal 'DayOfYear', or 'Nothing' on
-- invalid input.
--
-- See also 'Data.Thyme.Calendar.MonthDay.monthDay',
-- 'Data.Thyme.Calendar.MonthDay.monthDayValid',
-- 'monthAndDayToDayOfYear'.
{-# INLINE monthAndDayToDayOfYearValid #-}
monthAndDayToDayOfYearValid
    :: Bool
        -- ^ 'Data.Thyme.Calendar.isLeapYear'?
    -> Month
    -> DayOfMonth
    -> Maybe DayOfYear
monthAndDayToDayOfYearValid leap m d = monthDayValid leap (MonthDay m d)

------------------------------------------------------------------------
-- * @Data.Time.Calendar.OrdinalDate@

{-# INLINE toOrdinalDate #-}
-- | Convert a calendar 'Day' to ('Year', 'DayOfYear')
--
-- See also 'ordinalDate'.
toOrdinalDate :: Day -> (Year, DayOfYear)
toOrdinalDate (view ordinalDate -> OrdinalDate y d) = (y, d)

-- | Convert a 'Year' and 'DayOfYear' to a calendar 'Day'.
--
-- See also 'ordinalDate', 'ordinalDateValid', 'fromOrdinalDateValid'.
{-# INLINE fromOrdinalDate #-}
fromOrdinalDate :: Year -> DayOfYear -> Day
fromOrdinalDate y d = ordinalDate # OrdinalDate y d

-- | Convert a 'Year' and 'DayOfYear' to a calendar 'Day', or 'Nothing'
-- on invalid input.
--
-- See also 'ordinalDate', 'ordinalDateValid', 'fromOrdinalDate'.
{-# INLINE fromOrdinalDateValid #-}
fromOrdinalDateValid :: Year -> DayOfYear -> Maybe Day
fromOrdinalDateValid y d = ordinalDateValid (OrdinalDate y d)

-- | Get the number of the /Sunday/-starting week in the year and the day of
-- the week. The first /Sunday/ is the first day of week /1/, any earlier days
-- in the year are week /0/ (as @\"%U\"@ in 'Data.Thyme.Format.formatTime').
-- /Sunday/ is /0/, /Saturday/ is /6/
-- (as @\"%w\"@ in 'Data.Thyme.Format.formatTime').
--
-- See also 'sundayWeek', 'sundayWeekValid'.
{-# INLINE sundayStartWeek #-}
sundayStartWeek :: Day -> (Year, WeekOfYear, DayOfWeek)
sundayStartWeek (view sundayWeek -> SundayWeek y w d) = (y, w, d)

-- | The inverse of 'sundayStartWeek'.
--
-- See also 'sundayWeek', 'sundayWeekValid'.
{-# INLINE fromSundayStartWeek #-}
fromSundayStartWeek :: Year -> WeekOfYear -> DayOfWeek -> Day
fromSundayStartWeek y w d = sundayWeek # SundayWeek y w d

-- | The inverse of 'sundayStartWeek', returns 'Nothing' for invalid input.
--
-- See also 'sundayWeek', 'sundayWeekValid'.
{-# INLINE fromSundayStartWeekValid #-}
fromSundayStartWeekValid :: Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromSundayStartWeekValid y w d = sundayWeekValid (SundayWeek y w d)

-- | Get the number of the Monday-starting week in the year and the day of
-- the week. The first /Monday/ is the first day of week /1/, any earlier days
-- in the year are week /0/ (as @\"%W\"@ in 'Data.Thyme.Format.formatTime').
-- /Monday/ is /1/, /Sunday/ is /7/
-- (as @\"%u\"@ in 'Data.Thyme.Format.formatTime').
--
-- See also 'mondayWeek', 'mondayWeekValid'.
{-# INLINE mondayStartWeek #-}
mondayStartWeek :: Day -> (Year, WeekOfYear, DayOfWeek)
mondayStartWeek (view mondayWeek -> MondayWeek y w d) = (y, w, d)

-- | The inverse of 'mondayStartWeek'.
--
-- See also 'mondayWeek', 'mondayWeekValid'.
{-# INLINE fromMondayStartWeek #-}
fromMondayStartWeek :: Year -> WeekOfYear -> DayOfWeek -> Day
fromMondayStartWeek y w d = mondayWeek # MondayWeek y w d

-- | The inverse of 'mondayStartWeek', returns 'Nothing' for invalid input.
--
-- See also 'mondayWeek', 'mondayWeekValid'.
{-# INLINE fromMondayStartWeekValid #-}
fromMondayStartWeekValid :: Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromMondayStartWeekValid y w d = mondayWeekValid (MondayWeek y w d)

------------------------------------------------------------------------
-- * @Data.Time.Calendar.WeekDate@

-- | Convert a 'Day' to ('Year', 'WeekOfYear', 'DayOfWeek') according to the
-- Iso 'weekDate'.
{-# INLINE toWeekDate #-}
toWeekDate :: Day -> (Year, WeekOfYear, DayOfWeek)
toWeekDate (view weekDate -> WeekDate y w d) = (y, w, d)

-- | Convert a 'Year' 'WeekOfYear' 'DayOfWeek' to a 'Day' according to the
-- Iso 'weekDate'.
--
-- See also 'fromWeekDateValid'.
{-# INLINE fromWeekDate #-}
fromWeekDate :: Year -> WeekOfYear -> DayOfWeek -> Day
fromWeekDate y w d = weekDate # WeekDate y w d

-- | Convert a 'Year' 'WeekOfYear' 'DayOfWeek' to a 'Day' according to the
-- Iso 'weekDateValid'.
--
-- See also 'fromWeekDate'.
{-# INLINE fromWeekDateValid #-}
fromWeekDateValid :: Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromWeekDateValid y w d = weekDateValid (WeekDate y w d)

------------------------------------------------------------------------
-- * @Data.Time.Clock@

-- |
-- @
-- 'getModJulianDate' ≡ 'view' 'modJulianDate'
-- @
{-# INLINE getModJulianDate #-}
getModJulianDate :: UniversalTime -> Rational
getModJulianDate = view modJulianDate

-- | Replacement for 'T.ModJulianDate'.
-- @
-- 'mkModJulianDate' ≡ 'review' 'modJulianDate'
-- @
{-# INLINE mkModJulianDate #-}
mkModJulianDate :: Rational -> UniversalTime
mkModJulianDate = review modJulianDate

-- | Construct a 'DiffTime' from seconds.
--
-- See also 'fromSeconds'.
{-# INLINE secondsToDiffTime #-}
secondsToDiffTime :: Int64 -> DiffTime
secondsToDiffTime a = DiffTime (Micro $ a * 1000000)

-- | Construct a 'DiffTime' from picoseconds, but only to microsecond precision.
-- (The bottom six orders of magnitude are discarded.)
--
-- See also 'microseconds'.
{-# INLINE picosecondsToDiffTime #-}
picosecondsToDiffTime :: Int64 -> DiffTime
picosecondsToDiffTime a = DiffTime . Micro $
    quot (a + signum a * 500000) 1000000

-- | Constructor for 'UTCTime'.
--
-- See also 'utcTime'.
{-# INLINE mkUTCTime #-}
mkUTCTime :: Day -> DiffTime -> UTCTime
mkUTCTime d t = utcTime # UTCView d t

-- | Decompose a 'UTCTime' into a 'UTCView'.
--
-- See also 'utcTime'.
{-# INLINE unUTCTime #-}
unUTCTime :: UTCTime -> UTCView
unUTCTime = view utcTime

-- | Add a duration to a time point.
--
-- See also "Data.AffineSpace" instance of 'UTCTime'.
{-# INLINE addUTCTime #-}
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime = flip (.+^)

-- | The duration difference between two time points.
--
-- See also "Data.AffineSpace" instance of 'UTCTime'.
{-# INLINE diffUTCTime #-}
diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime = (.-.)

-- | Convert a 'DiffTime' or 'NominalDiffTime' to microseconds.
--
-- See also 'microseconds'.
{-# INLINE toMicroseconds #-}
toMicroseconds :: (TimeDiff t) => t -> Int64
toMicroseconds = view microseconds

-- | Convert microseconds to a 'DiffTime' or 'NominalDiffTime'.
--
-- See also 'microseconds'.
{-# INLINE fromMicroseconds #-}
fromMicroseconds :: (TimeDiff t) => Int64 -> t
fromMicroseconds = review microseconds

------------------------------------------------------------------------
-- * @Data.Time.Clock.POSIX@

-- |
-- @
-- 'posixSecondsToUTCTime' ≡ 'review' 'posixTime'
-- @
{-# INLINE posixSecondsToUTCTime #-}
posixSecondsToUTCTime :: POSIXTime -> UTCTime
posixSecondsToUTCTime = review posixTime

-- |
-- @
-- 'utcTimeToPOSIXSeconds' ≡ 'view' 'posixTime'
-- @
{-# INLINE utcTimeToPOSIXSeconds #-}
utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
utcTimeToPOSIXSeconds = view posixTime

------------------------------------------------------------------------
-- * @Data.Time.Clock.TAI@

-- | Add a duration to an 'AbsoluteTime'.
--
-- See also the 'Data.AffineSpace.+^' operator for
-- the 'Data.AffineSpace.AffineSpace' instance of 'AbsoluteTime'.
{-# INLINE addAbsoluteTime #-}
addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime = flip (.+^)

-- | The duration difference between two 'AbsoluteTime's.
--
-- See also the 'Data.AffineSpace..-.' operator for
-- the 'Data.AffineSpace.AffineSpace' instance of 'AbsoluteTime'.
{-# INLINE diffAbsoluteTime #-}
diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime = (.-.)

-- | Using a 'LeapSecondTable', convert a 'UTCTime' to 'AbsoluteTime'.
--
-- See also the Iso 'absoluteTime'.
{-# INLINE utcToTAITime #-}
utcToTAITime :: LeapSecondTable -> UTCTime -> AbsoluteTime
utcToTAITime = view . absoluteTime

-- | Using a 'LeapSecondTable', convert a 'AbsoluteTime' to 'UTCTime'.
--
-- See also the Iso 'absoluteTime'.
{-# INLINE taiToUTCTime #-}
taiToUTCTime :: LeapSecondTable -> AbsoluteTime -> UTCTime
taiToUTCTime = review . absoluteTime

------------------------------------------------------------------------
-- * @Data.Time.LocalTime@

-- | Convert a UTC time-of-day to a local time-of-day.
--
-- @
-- 'utcToLocalTimeOfDay' ≡ 'Data.Thyme.LocalTime.addMinutes' . 'Data.Thyme.LocalTime.timeZoneMinutes'
-- @
--
-- See also Iso 'Data.Thyme.LocalTime.utcLocalTime'
{-# INLINE utcToLocalTimeOfDay #-}
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Days, TimeOfDay)
utcToLocalTimeOfDay = addMinutes . timeZoneMinutes

-- | Convert a local time to a UTC time.
--
-- @
-- 'localToUTCTimeOfDay' ≡ 'Data.Thyme.LocalTime.addMinutes' . 'negate' . 'Data.Thyme.LocalTime.timeZoneMinutes'
-- @
--
-- See also Iso 'Data.Thyme.LocalTime.utcLocalTime'
{-# INLINE localToUTCTimeOfDay #-}
localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Days, TimeOfDay)
localToUTCTimeOfDay = addMinutes . negate . timeZoneMinutes

-- |
-- @
-- 'timeToTimeOfDay' ≡ 'view' 'Data.Thyme.LocalTime.timeOfDay'
-- @
{-# INLINE timeToTimeOfDay #-}
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay = view timeOfDay

-- | Convert 'TimeOfDay' to 'DiffTime'.
--
-- See also 'Data.Thyme.LocalTime.timeOfDay'.
{-# INLINE timeOfDayToTime #-}
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime = review timeOfDay

-- |
-- @
-- 'dayFractionToTimeOfDay' ≡ 'review' 'Data.Thyme.LocalTime.dayFraction'
-- @
{-# INLINE dayFractionToTimeOfDay #-}
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay = review dayFraction

-- |
-- @
-- 'timeOfDayToDayFraction' ≡ 'view' 'Data.Thyme.LocalTime.dayFraction'
-- @
{-# INLINE timeOfDayToDayFraction #-}
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction = view dayFraction

-- |
-- @
-- 'utcToLocalTime' ≡ 'view' . 'Data.Thyme.LocalTime.utcLocalTime'
-- @
{-# INLINE utcToLocalTime #-}
utcToLocalTime :: TimeZone -> UTCTime -> LocalTime
utcToLocalTime = view . utcLocalTime

-- |
-- @
-- 'localTimeToUTC' ≡ 'review' . 'Data.Thyme.LocalTime.utcLocalTime'
-- @
{-# INLINE localTimeToUTC #-}
localTimeToUTC :: TimeZone -> LocalTime -> UTCTime
localTimeToUTC = review . utcLocalTime

-- |
-- @
-- 'ut1ToLocalTime' ≡ 'view' . 'Data.Thyme.LocalTime.ut1LocalTime'
-- @
{-# INLINE ut1ToLocalTime #-}
ut1ToLocalTime :: Rational -> UniversalTime -> LocalTime
ut1ToLocalTime = view . ut1LocalTime

-- |
-- @
-- 'localTimeToUT1' ≡ 'review' . 'Data.Thyme.LocalTime.ut1LocalTime'
-- @
{-# INLINE localTimeToUT1 #-}
localTimeToUT1 :: Rational -> LocalTime -> UniversalTime
localTimeToUT1 = review . ut1LocalTime

-- | Combine a 'TimeZone' and a 'UTCTime' into a 'ZonedTime'.
--
-- See also Iso 'Data.Thyme.LocalTime.zonedTime'
{-# INLINE utcToZonedTime #-}
utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime
utcToZonedTime z t = view zonedTime (z, t)

-- | Convert a 'ZonedTime' to a 'UTCTime'.
--
-- See also Iso 'Data.Thyme.LocalTime.zonedTime'
{-# INLINE zonedTimeToUTC #-}
zonedTimeToUTC :: ZonedTime -> UTCTime
zonedTimeToUTC = snd . review zonedTime

