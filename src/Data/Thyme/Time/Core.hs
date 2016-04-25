{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

#if HLINT
#include "cabal_macros.h"
#endif

-- | This module provides the 'Thyme' typeclass, and instances for
-- converting between "Data.Time" and "Data.Thyme" types. It also provides
-- compatibility wrappers for existing code using "Data.Time".
--
-- Note that we do not provide 'Num' hierarchy instances for 'DiffTime' nor
-- 'NominalDiffTime' here. If you want to use them anyway despite parts of
-- them being ill-defined (e.g. @('*')@ on 'DiffTime'), import
-- "Data.Thyme.Time" instead.

module Data.Thyme.Time.Core
    ( module Data.Thyme
    , module Data.Thyme.Time.Core
    ) where

import Prelude
import Control.Lens
import Data.AffineSpace
import Data.Fixed
import Data.Int
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

-- | Typeclass for converting between "Data.Time" and "Data.Thyme" types.
class Thyme time thyme | thyme -> time where
    -- | Convert between "Data.Time" and "Data.Thyme" types.
    --
    -- @
    -- > :set -t
    -- > import qualified "Data.Time"
    --
    -- > 'thyme' 'Control.Lens.#' ('fromSeconds'' 10 :: 'DiffTime')
    -- 10s
    -- it :: 'Data.Time.DiffTime'
    --
    -- > 'Data.Time.secondsToDiffTime' 10 '^.' 'thyme' :: 'DiffTime'
    -- 10s
    -- it :: 'DiffTime'
    -- @
    thyme :: Iso' time thyme

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

-- | Convert a "Data.Time" type to a "Data.Thyme" type, if you would rather
-- not use "Control.Lens" directly.
--
-- @
-- 'toThyme' = 'view' 'thyme'
-- 'toThyme' t ≡ t '^.' 'thyme'
-- @
{-# INLINE toThyme #-}
toThyme :: (Thyme time thyme) => time -> thyme
toThyme = view thyme

-- | Convert a "Data.Thyme" type to a "Data.Time" type, if you would rather
-- not use "Control.Lens" directly.
--
-- @
-- 'fromThyme' = 'review' 'thyme'
-- 'fromThyme' t ≡ 'thyme' 'Control.Lens.#' t
-- @
{-# INLINE fromThyme #-}
fromThyme :: (Thyme time thyme) => thyme -> time
fromThyme = review thyme

------------------------------------------------------------------------
-- * @Data.Time.Calendar@

-- | Add some 'Days' to a calendar 'Day' to get a new 'Day'.
--
-- @
-- 'addDays' = 'flip' ('.+^')
-- 'addDays' n d ≡ d '.+^' n
-- @
--
-- See also the 'AffineSpace' instance for 'Day'.
{-# INLINE addDays #-}
addDays :: Days -> Day -> Day
addDays = flip (.+^)

-- | Subtract two calendar 'Day's for the difference in 'Days'.
--
-- @
-- 'diffDays' = ('.-.')
-- 'diffDays' a b = a '.-.' b
-- @
--
-- See also the 'AffineSpace' instance for 'Day'.
{-# INLINE diffDays #-}
diffDays :: Day -> Day -> Days
diffDays = (.-.)

-- | Convert a 'Day' to its Gregorian 'Year', 'Month', and 'DayOfMonth'.
--
-- @
-- 'toGregorian' ('view' 'gregorian' -> 'YearMonthDay' y m d) = (y, m, d)
-- @
{-# INLINE toGregorian #-}
toGregorian :: Day -> (Year, Month, DayOfMonth)
toGregorian (view gregorian -> YearMonthDay y m d) = (y, m, d)

-- | Construct a 'Day' from a Gregorian calendar date.
-- Does not validate the input.
--
-- @
-- 'fromGregorian' y m d = 'gregorian' 'Control.Lens.#' 'YearMonthDay' y m d
-- @
{-# INLINE fromGregorian #-}
fromGregorian :: Year -> Month -> DayOfMonth -> Day
fromGregorian y m d = gregorian # YearMonthDay y m d

-- | Construct a 'Day' from a Gregorian calendar date.
-- Returns 'Nothing' for invalid input.
--
-- @
-- 'fromGregorianValid' y m d = 'gregorianValid' ('YearMonthDay' y m d)
-- @
{-# INLINE fromGregorianValid #-}
fromGregorianValid :: Year -> Month -> DayOfMonth -> Maybe Day
fromGregorianValid y m d = gregorianValid (YearMonthDay y m d)

-- | Add some number of 'Months' to the given 'Day'; if the original
-- 'DayOfMonth' exceeds that of the new 'Month', it will be clipped to the
-- last day of the new 'Month'.
--
-- @
-- 'addGregorianMonthsClip' n = 'gregorian' '%~' 'gregorianMonthsClip' n
-- @
{-# INLINE addGregorianMonthsClip #-}
addGregorianMonthsClip :: Months -> Day -> Day
addGregorianMonthsClip n = gregorian %~ gregorianMonthsClip n

-- | Add some number of 'Months' to the given 'Day'; if the original
-- 'DayOfMonth' exceeds that of the new 'Month', it will be rolled over into
-- the following 'Month'.
--
-- @
-- 'addGregorianMonthsRollover' n = 'gregorian' '%~' 'gregorianMonthsRollover' n
-- @
{-# INLINE addGregorianMonthsRollover #-}
addGregorianMonthsRollover :: Months -> Day -> Day
addGregorianMonthsRollover n = gregorian %~ gregorianMonthsRollover n

-- | Add some number of 'Years' to the given 'Day', with /February 29th/
-- clipped to /February 28th/ if necessary.
--
-- @
-- 'addGregorianYearsClip' n = 'gregorian' '%~' 'gregorianYearsClip' n
-- @
{-# INLINE addGregorianYearsClip #-}
addGregorianYearsClip :: Years -> Day -> Day
addGregorianYearsClip n = gregorian %~ gregorianYearsClip n

-- | Add some number of 'Years' to the given 'Day', with /February 29th/
-- rolled over to /March 1st/ if necessary.
--
-- @
-- 'addGregorianYearsRollover' n = 'gregorian' '%~' 'gregorianYearsRollover' n
-- @
{-# INLINE addGregorianYearsRollover #-}
addGregorianYearsRollover :: Years -> Day -> Day
addGregorianYearsRollover n = gregorian %~ gregorianYearsRollover n

------------------------------------------------------------------------
-- * @Data.Time.Calendar.MonthDay@

-- | Predicated on whether or not it is a leap year, convert an ordinal
-- 'DayOfYear' to its corresponding 'Month' and 'DayOfMonth'.
--
-- @
-- 'dayOfYearToMonthAndDay' leap ('view' ('monthDay' leap) -> 'MonthDay' m d) = (m, d)
-- @
{-# INLINE dayOfYearToMonthAndDay #-}
dayOfYearToMonthAndDay
    :: Bool -- ^ 'isLeapYear'?
    -> DayOfYear
    -> (Month, DayOfMonth)
dayOfYearToMonthAndDay leap (view (monthDay leap) -> MonthDay m d) = (m, d)

-- | Predicated on whether or not it is a leap year, convert a 'Month' and
-- 'DayOfMonth' to its corresponding ordinal 'DayOfYear'.
-- Does not validate the input.
--
-- @
-- 'monthAndDayToDayOfYear' leap m d = 'monthDay' leap 'Control.Lens.#' 'MonthDay' m d
-- @
{-# INLINE monthAndDayToDayOfYear #-}
monthAndDayToDayOfYear
    :: Bool -- ^ 'isLeapYear'?
    -> Month
    -> DayOfMonth
    -> DayOfYear
monthAndDayToDayOfYear leap m d = monthDay leap # MonthDay m d

-- | Predicated on whether or not it is a leap year, convert a 'Month' and
-- 'DayOfMonth' to its corresponding ordinal 'DayOfYear'.
-- Returns 'Nothing' for invalid input.
--
-- @
-- 'monthAndDayToDayOfYearValid' leap m d = 'monthDayValid' leap ('MonthDay' m d)
-- @
{-# INLINE monthAndDayToDayOfYearValid #-}
monthAndDayToDayOfYearValid
    :: Bool -- ^ 'isLeapYear'?
    -> Month
    -> DayOfMonth
    -> Maybe DayOfYear
monthAndDayToDayOfYearValid leap m d = monthDayValid leap (MonthDay m d)

------------------------------------------------------------------------
-- * @Data.Time.Calendar.OrdinalDate@

{-# INLINE toOrdinalDate #-}
-- | Convert a 'Day' to its Gregorian 'Year' and 'DayOfYear'.
--
-- @
-- 'toOrdinalDate' ('view' 'ordinalDate' -> 'OrdinalDate' y d) = (y, d)
-- @
toOrdinalDate :: Day -> (Year, DayOfYear)
toOrdinalDate (view ordinalDate -> OrdinalDate y d) = (y, d)

-- | Convert a Gregorian 'Year' and 'DayOfYear' to a 'Day'.
-- Does not validate the input.
--
-- @
-- 'fromOrdinalDate' y d = 'ordinalDate' 'Control.Lens.#' 'OrdinalDate' y d
-- @
{-# INLINE fromOrdinalDate #-}
fromOrdinalDate :: Year -> DayOfYear -> Day
fromOrdinalDate y d = ordinalDate # OrdinalDate y d

-- | Converts a Gregorian 'Year' and 'DayOfYear' to a 'Day'.
-- Returns 'Nothing' on invalid input.
--
-- @
-- 'fromOrdinalDateValid' y d = 'ordinalDateValid' ('OrdinalDate' y d)
-- @
{-# INLINE fromOrdinalDateValid #-}
fromOrdinalDateValid :: Year -> DayOfYear -> Maybe Day
fromOrdinalDateValid y d = ordinalDateValid (OrdinalDate y d)

-- | Converts a 'Day' to its /Sunday/-starting week date.
--
-- The first /Sunday/ of the year belongs to @1 ∷ 'WeekOfYear'@; earlier
-- days in the same year are week @0@. This corresponds to @\"%U\"@ for
-- 'formatTime'.
--
-- /Sunday/ is @0 ∷ 'DayOfWeek'@, /Saturday/ is @6@. This corresponds to
-- @\"%w\"@ for 'formatTime'.
--
-- @
-- 'sundayStartWeek' ('view' 'sundayWeek' -> 'SundayWeek' y w d) = (y, w, d)
-- @
{-# INLINE sundayStartWeek #-}
sundayStartWeek :: Day -> (Year, WeekOfYear, DayOfWeek)
sundayStartWeek (view sundayWeek -> SundayWeek y w d) = (y, w, d)

-- | Converts a /Sunday/-starting week date to the corresponding 'Day'; the
-- inverse of 'sundayStartWeek'.
-- Does not validate the input.
--
-- @
-- 'fromSundayStartWeek' y w d = 'sundayWeek' 'Control.Lens.#' 'SundayWeek' y w d
-- @
{-# INLINE fromSundayStartWeek #-}
fromSundayStartWeek :: Year -> WeekOfYear -> DayOfWeek -> Day
fromSundayStartWeek y w d = sundayWeek # SundayWeek y w d

-- | Converts a /Sunday/-starting week date to the corresponding 'Day'; the
-- inverse of 'sundayStartWeek'.
-- Returns 'Nothing' for invalid input.
--
-- @
-- 'fromSundayStartWeekValid' y w d = 'sundayWeekValid' ('SundayWeek' y w d)
-- @
{-# INLINE fromSundayStartWeekValid #-}
fromSundayStartWeekValid :: Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromSundayStartWeekValid y w d = sundayWeekValid (SundayWeek y w d)

-- | Converts a 'Day' to its /Monday/-starting week date.
--
-- The first /Monday/ of the year belongs to @1 ∷ 'WeekOfYear'@; earlier
-- days in the same year are week @0@. This corresponds to @\"%W\"@ for
-- 'formatTime'.
--
-- /Monday/ is @1 ∷ 'DayOfWeek'@, /Sunday/ is @7@. This corresponds to
-- @\"%u\"@ for 'formatTime'.
--
-- @
-- 'mondayStartWeek' ('view' 'mondayWeek' -> 'MondayWeek' y w d) = (y, w, d)
-- @
{-# INLINE mondayStartWeek #-}
mondayStartWeek :: Day -> (Year, WeekOfYear, DayOfWeek)
mondayStartWeek (view mondayWeek -> MondayWeek y w d) = (y, w, d)

-- | Converts a /Monday/-starting week date to the corresponding 'Day'; the
-- inverse of 'mondayStartWeek'.
-- Does not validate the input.
--
-- @
-- 'fromMondayStartWeek' y w d = 'mondayWeek' 'Control.Lens.#' 'MondayWeek' y w d
-- @
{-# INLINE fromMondayStartWeek #-}
fromMondayStartWeek :: Year -> WeekOfYear -> DayOfWeek -> Day
fromMondayStartWeek y w d = mondayWeek # MondayWeek y w d

-- | Converts a /Monday/-starting week date to the corresponding 'Day'; the
-- inverse of 'mondayStartWeek'.
-- Returns 'Nothing' for invalid input.
--
-- @
-- 'fromMondayStartWeekValid' y w d = 'mondayWeekValid' ('MondayWeek' y w d)
-- @
{-# INLINE fromMondayStartWeekValid #-}
fromMondayStartWeekValid :: Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromMondayStartWeekValid y w d = mondayWeekValid (MondayWeek y w d)

------------------------------------------------------------------------
-- * @Data.Time.Calendar.WeekDate@

-- | Converts a 'Day' to an <https://en.wikipedia.org/wiki/ISO_week_date ISO week date>.
--
-- @
-- 'toWeekDate' ('view' 'weekDate' -> 'WeekDate' y w d) = (y, w, d)
-- @
{-# INLINE toWeekDate #-}
toWeekDate :: Day -> (Year, WeekOfYear, DayOfWeek)
toWeekDate (view weekDate -> WeekDate y w d) = (y, w, d)

-- | Converts an <https://en.wikipedia.org/wiki/ISO_week_date ISO week date>
-- to a 'Day'.
-- Does not validate the input.
--
-- @
-- 'fromWeekDate' y w d = 'weekDate' 'Control.Lens.#' 'WeekDate' y w d
-- @
{-# INLINE fromWeekDate #-}
fromWeekDate :: Year -> WeekOfYear -> DayOfWeek -> Day
fromWeekDate y w d = weekDate # WeekDate y w d

-- | Converts an <https://en.wikipedia.org/wiki/ISO_week_date ISO week date>
-- to a 'Day'.
-- Returns 'Nothing' for invalid input.
--
-- @
-- 'fromWeekDateValid' y w d = 'weekDateValid' ('WeekDate' y w d)
-- @
{-# INLINE fromWeekDateValid #-}
fromWeekDateValid :: Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromWeekDateValid y w d = weekDateValid (WeekDate y w d)

------------------------------------------------------------------------
-- * @Data.Time.Clock@

-- | Convert a 'UniversalTime' to the fractional number of days since the
-- <http://en.wikipedia.org/wiki/Julian_day#Variants Modified Julian Date epoch>.
--
-- @
-- 'getModJulianDate' = 'view' 'modJulianDate'
-- @
{-# INLINE getModJulianDate #-}
getModJulianDate :: UniversalTime -> Rational
getModJulianDate = view modJulianDate

-- | Construct a 'UniversalTime' from the fractional number of days since the
-- <http://en.wikipedia.org/wiki/Julian_day#Variants Modified Julian Date epoch>.
--
-- @
-- 'mkModJulianDate' = 'review' 'modJulianDate'
-- @
{-# INLINE mkModJulianDate #-}
mkModJulianDate :: Rational -> UniversalTime
mkModJulianDate = review modJulianDate

-- | Construct a 'DiffTime' from some number of seconds.
--
-- This is just 'fromSeconds' with a more constrained type.
--
-- @
-- 'secondsToDiffTime' = 'fromSeconds'
-- @
{-# INLINE secondsToDiffTime #-}
secondsToDiffTime :: Int64 -> DiffTime
secondsToDiffTime = fromSeconds

-- | Construct a 'DiffTime' from some number of picoseconds.
-- The input will be rounded to the nearest microsecond.
--
-- @
-- 'picosecondsToDiffTime' a = 'microseconds' 'Control.Lens.#' 'quot' (a '+' 'signum' a '*' 500000) 1000000
-- @
{-# INLINE picosecondsToDiffTime #-}
picosecondsToDiffTime :: Int64 -> DiffTime
picosecondsToDiffTime a = microseconds # quot (a + signum a * 500000) 1000000

-- | Decompose a 'UTCTime' into a 'UTCView'.
--
-- @
-- 'unUTCTime' = 'view' 'utcTime'
-- @
--
-- For GHC 7.8 or later, there is also the pattern synonym
-- @<Data-Thyme-Clock.html#v:UTCTime UTCTime>@.
{-# INLINE unUTCTime #-}
unUTCTime :: UTCTime -> UTCView
unUTCTime = view utcTime

-- | Add a duration to a point in time.
--
-- @
-- 'addUTCTime' = 'flip' ('.+^')
-- 'addUTCTime' d t ≡ t '.+^' d
-- @
--
-- See also the 'AffineSpace' instance for 'UTCTime'.
{-# INLINE addUTCTime #-}
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime = flip (.+^)

-- | The duration difference between two time points.
--
-- @
-- 'diffUTCTime' = ('.-.')
-- 'diffUTCTime' a b = a '.-.' b
-- @
--
-- See also the 'AffineSpace' instance for 'UTCTime'.
{-# INLINE diffUTCTime #-}
diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime = (.-.)

-- | The number of microseconds in a 'DiffTime' or 'NominalDiffTime'.
--
-- @
-- 'toMicroseconds' :: 'DiffTime' -> 'Int64'
-- 'toMicroseconds' :: 'NominalDiffTime' -> 'Int64'
-- 'toMicroseconds' = 'view' 'microseconds'
-- 'toMicroseconds' d ≡ d '^.' 'microseconds'
-- @
{-# INLINE toMicroseconds #-}
toMicroseconds :: (TimeDiff t) => t -> Int64
toMicroseconds = view microseconds

-- | Construct a 'DiffTime' or 'NominalDiffTime' from a number of
-- microseconds.
--
-- @
-- 'fromMicroseconds' :: 'Int64' -> 'DiffTime'
-- 'fromMicroseconds' :: 'Int64' -> 'NominalDiffTime'
-- 'fromMicroseconds' = 'review' 'microseconds'
-- 'fromMicroseconds' n ≡ 'microseconds' 'Control.Lens.#' n
-- @
{-# INLINE fromMicroseconds #-}
fromMicroseconds :: (TimeDiff t) => Int64 -> t
fromMicroseconds = review microseconds

------------------------------------------------------------------------
-- * @Data.Time.Clock.POSIX@

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

------------------------------------------------------------------------
-- * @Data.Time.Clock.TAI@

-- | Add a duration to an 'AbsoluteTime'.
--
-- @
-- 'addAbsoluteTime' = 'flip' ('.+^')
-- 'addAbsoluteTime' d t ≡ t '.+^' d
-- @
--
-- See also the 'AffineSpace' instance for 'AbsoluteTime'.
{-# INLINE addAbsoluteTime #-}
addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime = flip (.+^)

-- | The duration difference between two 'AbsoluteTime's.
--
-- @
-- 'diffAbsoluteTime' = ('.-.')
-- 'diffAbsoluteTime' a b ≡ a '.-.' b
-- @
--
-- See also the 'AffineSpace' instance for 'AbsoluteTime'.
{-# INLINE diffAbsoluteTime #-}
diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime = (.-.)

-- | Using a 'LeapSecondTable', convert a 'UTCTime' to 'AbsoluteTime'.
--
-- @
-- 'utcToTAITime' = 'view' '.' 'absoluteTime'
-- @
{-# INLINE utcToTAITime #-}
utcToTAITime :: LeapSecondTable -> UTCTime -> AbsoluteTime
utcToTAITime = view . absoluteTime

-- | Using a 'LeapSecondTable', convert a 'AbsoluteTime' to 'UTCTime'.
--
-- @
-- 'taiToUTCTime' = 'review' '.' 'absoluteTime'
-- @
{-# INLINE taiToUTCTime #-}
taiToUTCTime :: LeapSecondTable -> AbsoluteTime -> UTCTime
taiToUTCTime = review . absoluteTime

------------------------------------------------------------------------
-- * @Data.Time.LocalTime@

-- | Convert a UTC 'TimeOfDay' to a 'TimeOfDay' in some timezone, together
-- with a day adjustment.
--
-- @
-- 'utcToLocalTimeOfDay' = 'addMinutes' '.' 'timeZoneMinutes'
-- @
{-# INLINE utcToLocalTimeOfDay #-}
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Days, TimeOfDay)
utcToLocalTimeOfDay = addMinutes . timeZoneMinutes

-- | Convert a 'TimeOfDay' in some timezone to a UTC 'TimeOfDay', together
-- with a day adjustment.
--
-- @
-- 'localToUTCTimeOfDay' = 'addMinutes' '.' 'negate' '.' 'timeZoneMinutes'
-- @
{-# INLINE localToUTCTimeOfDay #-}
localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Days, TimeOfDay)
localToUTCTimeOfDay = addMinutes . negate . timeZoneMinutes

-- | Convert a 'DiffTime' of the duration since midnight to a 'TimeOfDay'.
-- Durations exceeding 24 hours will be treated as leap-seconds.
--
-- @
-- 'timeToTimeOfDay' = 'view' 'timeOfDay'
-- 'timeToTimeOfDay' d ≡ d '^.' 'timeOfDay'
-- @
{-# INLINE timeToTimeOfDay #-}
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay = view timeOfDay

-- | Convert a 'TimeOfDay' to a 'DiffTime' of the duration since midnight.
-- 'TimeOfDay' greater than 24 hours will be treated as leap-seconds.
--
-- @
-- 'timeOfDayToTime' = 'review' 'timeOfDay'
-- 'timeOfDayToTime' tod ≡ 'timeOfDay' 'Control.Lens.#' tod
-- @
{-# INLINE timeOfDayToTime #-}
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime = review timeOfDay

-- | Convert a fraction of a day since midnight to a 'TimeOfDay'.
--
-- @
-- 'dayFractionToTimeOfDay' = 'review' 'dayFraction'
-- @
{-# INLINE dayFractionToTimeOfDay #-}
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay = review dayFraction

-- | Convert a 'TimeOfDay' to a fraction of a day since midnight.
--
-- @
-- 'timeOfDayToDayFraction' = 'view' 'dayFraction'
-- @
{-# INLINE timeOfDayToDayFraction #-}
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction = view dayFraction

-- | Convert a 'UTCTime' to a 'LocalTime' in the given 'TimeZone'.
--
-- @
-- 'utcToLocalTime' = 'view' '.' 'utcLocalTime'
-- @
{-# INLINE utcToLocalTime #-}
utcToLocalTime :: TimeZone -> UTCTime -> LocalTime
utcToLocalTime = view . utcLocalTime

-- | Convert a 'LocalTime' in the given 'TimeZone' to a 'UTCTime'.
--
-- @
-- 'localTimeToUTC' = 'review' '.' 'utcLocalTime'
-- @
{-# INLINE localTimeToUTC #-}
localTimeToUTC :: TimeZone -> LocalTime -> UTCTime
localTimeToUTC = review . utcLocalTime

-- | Convert a 'UniversalTime' to a 'LocalTime' at the given medidian in
-- degrees East.
--
-- @
-- 'ut1ToLocalTime' = 'view' '.' 'ut1LocalTime'
-- @
{-# INLINE ut1ToLocalTime #-}
ut1ToLocalTime :: Rational -> UniversalTime -> LocalTime
ut1ToLocalTime = view . ut1LocalTime

-- | Convert a 'LocalTime' at the given meridian in degrees East to
-- a 'UniversalTime'.
--
-- @
-- 'localTimeToUT1' = 'review' '.' 'ut1LocalTime'
-- @
{-# INLINE localTimeToUT1 #-}
localTimeToUT1 :: Rational -> LocalTime -> UniversalTime
localTimeToUT1 = review . ut1LocalTime

-- | Convert a 'UTCTime' and the given 'TimeZone' into a 'ZonedTime'.
--
-- @
-- 'utcToZonedTime' z t = 'view' 'zonedTime' (z, t)
-- @
{-# INLINE utcToZonedTime #-}
utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime
utcToZonedTime z t = view zonedTime (z, t)

-- | Converts a 'ZonedTime' to a 'UTCTime'.
--
-- @
-- 'zonedTimeToUTC' = 'snd' '.' 'review' 'zonedTime'
-- @
{-# INLINE zonedTimeToUTC #-}
zonedTimeToUTC :: ZonedTime -> UTCTime
zonedTimeToUTC = snd . review zonedTime

