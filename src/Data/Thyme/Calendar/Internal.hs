{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
#if SHOW_INTERNAL
{-# LANGUAGE StandaloneDeriving #-}
#endif
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

#if HLINT
#include "cabal_macros.h"
#endif

module Data.Thyme.Calendar.Internal where

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Bits
import Data.Data
import Data.Hashable
import Data.Int
import Data.Ix
import Data.Thyme.Format.Internal
#if __GLASGOW_HASKELL__ == 704
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
#endif
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import System.Random
import Test.QuickCheck hiding ((.&.))

-- | A duration/count of years.
type Years = Int

-- | A duration/count of months.
type Months = Int

-- | A duration/count of days.
type Days = Int

-- | A calendar-agnostic day, internally represented as a count of days
-- since /1858-11-17/, the
-- <https://en.wikipedia.org/wiki/Julian_day#Variants Modified Julian Day>
-- (MJD) epoch.
--
-- To convert a 'Day' to the corresponding 'YearMonthDay' in the <https://en.wikipedia.org/wiki/Gregorian_calendar Gregorian>
-- calendar, see 'gregorian'.
--
-- @
-- > 'gregorian' 'Control.Lens.#' 'YearMonthDay' 2016 3 1
-- 2016-03-01
-- @
--
-- 'Day' is an instance of 'AffineSpace' where @'Diff' 'Day' = 'Days'@, so
-- arithmetic on 'Day' and 'Days' can be performed with the '.-.', '.+^',
-- and '.-^' operators.
--
-- @
-- > 'gregorian' 'Control.Lens.#' 'YearMonthDay' 2016 3 1  '.-.'  'gregorian' 'Control.Lens.#' 'YearMonthDay' 2016 2 1
-- 29
-- @
--
-- @
-- > 'gregorian' 'Control.Lens.#' 'YearMonthDay' 2016 3 1 '.-^' 1
-- 2016-02-29
-- @
--
-- Other ways of viewing a 'Day' include 'ordinalDate', and 'weekDate'.
newtype Day = ModifiedJulianDay
    { toModifiedJulianDay :: Int
    } deriving (Eq, Ord, Data, Typeable, Generic, Enum, Ix, Hashable, NFData, CoArbitrary)

instance AffineSpace Day where
    type Diff Day = Days
    {-# INLINE (.-.) #-}
    (.-.) = \ (ModifiedJulianDay a) (ModifiedJulianDay b) -> a - b
    {-# INLINE (.+^) #-}
    (.+^) = \ (ModifiedJulianDay a) d -> ModifiedJulianDay (a + d)

-- | Convert between a 'Day' and the corresponding count of days from
-- 1858-11-17, the MJD epoch.
--
-- @
-- 'modifiedJulianDay' = 'iso' 'toModifiedJulianDay' 'ModifiedJulianDay'
-- @
--
-- @
-- > 'modifiedJulianDay' 'Control.Lens.#' 0
-- 1858-11-17
-- > 'gregorian' 'Control.Lens.#' 'YearMonthDay' 2016 3 1 '&' 'modifiedJulianDay' '%~' 'subtract' 1
-- 2016-02-29
-- @
{-# INLINE modifiedJulianDay #-}
modifiedJulianDay :: Iso' Day Int
modifiedJulianDay = iso toModifiedJulianDay ModifiedJulianDay

-- | Conversion between a <https://en.wikipedia.org/wiki/Gregorian_calendar Gregorian> 'OrdinalDate' and the corresponding
-- 'YearMonthDay'.
--
-- @
-- > 'OrdinalDate' 2016 32 '^.' 'yearMonthDay'
-- 'YearMonthDay' {ymdYear = 2016, ymdMonth = 2, ymdDay = 1}
-- @
--
-- @
-- > 'yearMonthDay' 'Control.Lens.#' 'YearMonthDay' 2016 2 1
-- 'OrdinalDate' {odYear = 2016, odDay = 32}
-- @
{-# INLINE yearMonthDay #-}
yearMonthDay :: Iso' OrdinalDate YearMonthDay
yearMonthDay = iso fromOrdinal toOrdinal where

    {-# INLINEABLE fromOrdinal #-}
    fromOrdinal :: OrdinalDate -> YearMonthDay
    fromOrdinal (OrdinalDate y yd) = YearMonthDay y m d where
        MonthDay m d = yd ^. monthDay (isLeapYear y)

    {-# INLINEABLE toOrdinal #-}
    toOrdinal :: YearMonthDay -> OrdinalDate
    toOrdinal (YearMonthDay y m d) = OrdinalDate y $
        monthDay (isLeapYear y) # MonthDay m d

-- | Conversion between a 'Day' and its 'YearMonthDay'.
--
-- @
-- 'gregorian' = 'ordinalDate' . 'yearMonthDay'
-- @
--
-- @
-- > 'ModifiedJulianDay' 0 '^.' 'gregorian'
-- 'YearMonthDay' {ymdYear = 1858, ymdMonth = 11, ymdDay = 17}
-- @
--
-- @
-- > 'gregorian' 'Control.Lens.#' 'YearMonthDay' 1858 11 17
-- 1858-11-17
-- @
{-# INLINE gregorian #-}
gregorian :: Iso' Day YearMonthDay
gregorian = ordinalDate . yearMonthDay

-- | Conversion between a 'YearMonthDay' and the corresponding 'Day'.
-- Returns 'Nothing' for invalid input.
--
-- @
-- > 'gregorianValid' ('YearMonthDay' 2015 2 28)
-- 'Just' 2015-02-28
-- @
--
-- @
-- > 'gregorianValid' ('YearMonthDay' 2015 2 29)
-- 'Nothing'
-- @
{-# INLINEABLE gregorianValid #-}
gregorianValid :: YearMonthDay -> Maybe Day
gregorianValid (YearMonthDay y m d) = review ordinalDate . OrdinalDate y
    <$> monthDayValid (isLeapYear y) (MonthDay m d)

-- | Shows a 'Day' in
-- <https://en.wikipedia.org/wiki/ISO_8601#Calendar_dates ISO 8601>
-- /YYYY-MM-DD/ format.
--
-- See "Data.Thyme.Format" for other possibilities.
{-# INLINEABLE showGregorian #-}
showGregorian :: Day -> String
showGregorian (view gregorian -> YearMonthDay y m d) =
    showsYear y . (:) '-' . shows02 m . (:) '-' . shows02 d $ ""

#if SHOW_INTERNAL
deriving instance Show Day
#else
instance Show Day where show = showGregorian
#endif

------------------------------------------------------------------------

-- | Calendar year.
type Year = Int

-- | Calendar month. /January = 1/
type Month = Int

-- | Calendar day-of-month, starting from /1/.
type DayOfMonth = Int

-- | A strict triple of a 'Year', a 'Day', and a 'Month'.
data YearMonthDay = YearMonthDay
    { ymdYear :: {-# UNPACK #-}!Year
    , ymdMonth :: {-# UNPACK #-}!Month
    , ymdDay :: {-# UNPACK #-}!DayOfMonth
    } deriving (Eq, Ord, Data, Typeable, Generic, Show)

instance Hashable YearMonthDay
instance NFData YearMonthDay

------------------------------------------------------------------------

-- | Is it a leap year according to the <https://en.wikipedia.org/wiki/Gregorian_calendar Gregorian> calendar?
isLeapYear :: Year -> Bool
isLeapYear y = y .&. 3 == 0 && (r100 /= 0 || q100 .&. 3 == 0) where
    (q100, r100) = y `quotRem` 100

-- | The day of the year, with /1 = January 1st/.
type DayOfYear = Int

-- | An
-- <https://en.wikipedia.org/wiki/ISO_8601#Ordinal_dates ISO 8601 ordinal date>.
data OrdinalDate = OrdinalDate
    { odYear :: {-# UNPACK #-}!Year
    , odDay :: {-# UNPACK #-}!DayOfYear
    } deriving (Eq, Ord, Data, Typeable, Generic, Show)

instance Hashable OrdinalDate
instance NFData OrdinalDate

-- | Conversion between the MJD 'Day' and 'OrdinalDate'.
--
-- @
-- > 'ordinalDate' 'Control.Lens.#' 'OrdinalDate' 2016 32
-- 2016-02-01
-- @
--
-- @
-- > 'toModifiedJulianDay' $ 'ordinalDate' 'Control.Lens.#' 'OrdinalDate' 2016 32
-- 57419
-- @
--
-- @
-- > 'ModifiedJulianDay' 57419 '^.' 'ordinalDate'
-- 'OrdinalDate' {odYear = 2016, odDay = 32}
-- @
{-# INLINE ordinalDate #-}
ordinalDate :: Iso' Day OrdinalDate
ordinalDate = iso toOrd fromOrd where

-- Brief description of the toOrd computation
--
-- The length of the years in the Gregorian calendar is periodic with period
-- of /400/ years. There are /100 - 4 + 1 = 97/ leap years in a period, so
-- the average length of a year is /365 + 97\/400 = 146097\/400/ days.
--
-- Now, if you consider these — let's call them nominal — years,
-- then for any point in time, for any linear day number we can
-- determine which nominal year does it fall into by a single
-- division. Moreover, if we align the start of the calendar year /1/
-- with the start of the nominal year /1/, then the calendar years and
-- nominal years never get too much out of sync. Specifically:
--
--  * The start of the first day of a calendar year might fall into the
--    preceding nominal year, but never more than by /1.5/ days (/591\/400/
--    days, to be precise).
--
--  * The start of the last day of a calendar year always falls into
--    its nominal year (even for the leap years).
--
-- So, to find out the calendar year for a given day, we calculate
-- on which nominal year does its start fall. And, if we are not too
-- close to the end of year, we have the right calendar
-- year. Othewise, we just check whether it falls within the next
-- calendar year.
--
-- Notes: to make the reasoning simpler and more efficient ('quot' is
-- faster than 'div') we do the computation directly only for positive
-- years (days after /0001-01-01/). For earlier dates we translate by an
-- integral number of /400/ year periods, do the computation and
-- translate back.

    {-# INLINEABLE toOrd #-}
    toOrd :: Day -> OrdinalDate
    toOrd (ModifiedJulianDay mjd)
      | dayB0 <= 0 = case toOrdB0 dayInQC of
        OrdinalDate y yd -> OrdinalDate (y + quadCent * 400) yd
      | otherwise = toOrdB0 dayB0
      where
        dayB0 = mjd + 678575
        (quadCent, dayInQC) = dayB0 `divMod` 146097

    -- Input: days since 0001-01-01. Precondition: has to be positive!
    {-# INLINE toOrdB0 #-}
    toOrdB0 :: Int -> OrdinalDate
    toOrdB0 dayB0 = res
      where
        (y0, r) = (400 * dayB0) `quotRem` 146097
        d0 = dayInYear y0 dayB0
        d1 = dayInYear (y0 + 1) dayB0
        res = if r > 146097 - 600 && d1 > 0
              then OrdinalDate (y0 + 1 + 1) d1
              else OrdinalDate (y0 + 1) d0

    -- Input: (year - 1) (day as days since 0001-01-01)
    -- Precondition: year is positive!
    {-# INLINE dayInYear #-}
    dayInYear :: Int -> Int -> Int
    dayInYear y0 dayB0 = dayB0 - 365 * y0 - leaps + 1
      where
        leaps = y0 `shiftR` 2 - centuries + centuries `shiftR` 2
        centuries = y0 `quot` 100

    {-# INLINEABLE fromOrd #-}
    fromOrd :: OrdinalDate -> Day
    fromOrd (OrdinalDate year yd) = ModifiedJulianDay mjd where
        years = year - 1
        centuries = years `div` 100
        leaps = years `shiftR` 2 - centuries + centuries `shiftR` 2
        mjd = 365 * years + leaps - 678576
            + clip 1 (if isLeapYear year then 366 else 365) yd
        clip a b = max a . min b

------------------------------------------------------------------------
-- Lookup tables for Data.Thyme.Calendar.MonthDay

{-# NOINLINE monthLengths #-}
{-# NOINLINE monthLengthsLeap #-}
monthLengths, monthLengthsLeap :: VU.Vector Days
monthLengths     = VU.fromList [31,28,31,30,31,30,31,31,30,31,30,31]
monthLengthsLeap = VU.fromList [31,29,31,30,31,30,31,31,30,31,30,31]
                             -- J  F  M  A  M  J  J  A  S  O  N  D

{-# ANN monthDays "HLint: ignore Use fromMaybe" #-}
{-# NOINLINE monthDays #-}
monthDays :: VU.Vector ({-Month-}Int8, {-DayOfMonth-}Int8)
monthDays = VU.generate 365 go where
    dom01 = VU.prescanl' (+) 0 monthLengths
    go yd = (fromIntegral m, fromIntegral d) where
        m = maybe 12 id $ VU.findIndex (yd <) dom01
        d = succ yd - VU.unsafeIndex dom01 (pred m)

{-# ANN monthDaysLeap "HLint: ignore Use fromMaybe" #-}
{-# NOINLINE monthDaysLeap #-}
monthDaysLeap :: VU.Vector ({-Month-}Int8, {-DayOfMonth-}Int8)
monthDaysLeap = VU.generate 366 go where
    dom01 = VU.prescanl' (+) 0 monthLengthsLeap
    go yd = (fromIntegral m, fromIntegral d) where
        m = maybe 12 id $ VU.findIndex (yd <) dom01
        d = succ yd - VU.unsafeIndex dom01 (pred m)

-- | No good home for this within the current hierarchy. This will do.
{-# INLINEABLE randomIsoR #-}
randomIsoR :: (Random s, RandomGen g) => Iso' s a -> (a, a) -> g -> (a, g)
randomIsoR l (x, y) = first (^. l) . randomR (l # x, l # y)

------------------------------------------------------------------------

-- | A strict pair of a 'Month' and a 'DayOfMonth'.
data MonthDay = MonthDay
    { mdMonth :: {-# UNPACK #-}!Month
    , mdDay :: {-# UNPACK #-}!DayOfMonth
    } deriving (Eq, Ord, Data, Typeable, Generic, Show)

instance Hashable MonthDay
instance NFData MonthDay

instance Bounded MonthDay where
    minBound = MonthDay 1 1
    maxBound = MonthDay 12 31

instance Random MonthDay where
    randomR r g = randomIsoR (monthDay leap) r g' where
        (isLeapYear -> leap, g') = random g
    random = randomR (minBound, maxBound)

instance Arbitrary MonthDay where
    arbitrary = choose (minBound, maxBound)
    shrink md = view (monthDay True) <$> shrink (monthDay True # md)

instance CoArbitrary MonthDay where
    coarbitrary (MonthDay m d) = coarbitrary m . coarbitrary d

-- | Predicated on whether or not it's a leap year, convert between an
-- ordinal 'DayOfYear' and the corresponding 'Month' and 'DayOfMonth'.
--
-- @
-- > 60 '^.' 'monthDay' ('isLeapYear' 2015)
-- 'MonthDay' {'mdMonth' = 3, 'mdDay' = 1}
-- @
--
-- @
-- > 60 '^.' 'monthDay' ('isLeapYear' 2016)
-- 'MonthDay' {'mdMonth' = 2, 'mdDay' = 29}
-- @
--
-- @
-- > 'monthDay' ('isLeapYear' 2016) 'Control.Lens.#' 'MonthDay' 2 29
-- 60
-- @
--
-- @
-- > 'monthDay' ('isLeapYear' 2015) 'Control.Lens.#' 'MonthDay' 2 28
-- 59
-- @
--
-- Note that 'monthDay' is an improper 'Iso', as the following example
-- shows. To handle this case correctly, use 'monthDayValid'.
--
-- @
-- > 'monthDay' ('isLeapYear' 2015) 'Control.Lens.#' 'MonthDay' 2 29
-- 59
-- @
{-# INLINE monthDay #-}
monthDay
    :: Bool -- ^ 'isLeapYear'?
    -> Iso' DayOfYear MonthDay
monthDay leap = iso fromOrdinal toOrdinal where
    (lastDay, lengths, table, ok) = if leap
        then (365, monthLengthsLeap, monthDaysLeap, -1)
        else (364, monthLengths, monthDays, -2)

    {-# INLINE fromOrdinal #-}
    fromOrdinal :: DayOfYear -> MonthDay
    fromOrdinal (max 0 . min lastDay . pred -> i) = MonthDay m d where
        (fromIntegral -> m, fromIntegral -> d) = VU.unsafeIndex table i

    {-# INLINE toOrdinal #-}
    toOrdinal :: MonthDay -> DayOfYear
    toOrdinal (MonthDay month day) = div (367 * m - 362) 12 + k + d where
        m = max 1 . min 12 $ month
        l = VU.unsafeIndex lengths (pred m)
        d = max 1 . min l $ day
        k = if m <= 2 then 0 else ok

-- | Predicated on whether or not it's a leap year, convert a 'MonthDay' to
-- an ordinal 'DayOfYear'.
--
-- @
-- > 'monthDayValid' ('isLeapYear' 2016) ('MonthDay' 2 29)
-- 'Just' 60
-- @
--
-- @
-- > 'monthDayValid' ('isLeapYear' 2015) ('MonthDay' 2 29)
-- 'Nothing'
-- @
{-# INLINEABLE monthDayValid #-}
monthDayValid
    :: Bool -- ^ 'isLeapYear'?
    -> MonthDay
    -> Maybe DayOfYear
monthDayValid leap md@(MonthDay m d) = monthDay leap # md
    <$ guard (1 <= m && m <= 12 && 1 <= d && d <= monthLength leap m)

-- | Predicated on whether or not the year is a leap year, return the number
-- of 'Days' in the given 'Month'.
--
-- @
-- > monthLength ('isLeapYear' 2015) 2
--   28
-- @
--
-- @
-- > monthLength ('isLeapYear' 2016) 2
--   29
-- @
{-# INLINEABLE monthLength #-}
monthLength
    :: Bool -- ^ 'isLeapYear'?
    -> Month
    -> Days
monthLength leap = VU.unsafeIndex ls . max 0 . min 11 . pred where
    ls = if leap then monthLengthsLeap else monthLengths

------------------------------------------------------------------------

-- | Week of the year.
--
-- Meaning of values depends on context; see 'wdWeek', 'swWeek', 'mwWeek'.
type WeekOfYear = Int

-- | Day of the week.
--
-- [/0/] /Sunday/ for 'SundayWeek'
--
-- [/1/…/6/] /Monday/…/Saturday/
--
-- [/7/] /Sunday/ for 'WeekDate', 'MondayWeek', and 'Data.Thyme.Calendar.WeekdayOfMonth.WeekdayOfMonth'
type DayOfWeek = Int

-- | <https://en.wikipedia.org/wiki/ISO_week_date ISO 8601 Week Date>.
--
-- Note that week /01/ is defined as the week with the first Thursday, thus
-- 'wdYear' may differ from the Gregorian year between /December 29th/ and
-- /January 3rd/.
data WeekDate = WeekDate
    { wdYear :: {-# UNPACK #-}!Year
    , wdWeek :: {-# UNPACK #-}!WeekOfYear
        -- ^ Numbered /01/ to /53/. Days before week /01/ are considered to
        -- belong to the previous year.
    , wdDay :: {-# UNPACK #-}!DayOfWeek
        -- ^ /1 = Monday/ … /7 = Sunday/.
    } deriving (Eq, Ord, Data, Typeable, Generic, Show)

instance Hashable WeekDate
instance NFData WeekDate

-- | Convert between a 'Day' and an ISO 8601 'WeekDate'.
--
-- @
-- > 'YearMonthDay' 2016 1 1 '^.' 'from' 'gregorian' '.' 'weekDate'
-- 'WeekDate' {'wdYear' = 2015, 'wdWeek' = 53, 'wdDay' = 5}
-- @
{-# INLINE weekDate #-}
weekDate :: Iso' Day WeekDate
weekDate = iso toWeek fromWeek where

    {-# INLINEABLE toWeek #-}
    toWeek :: Day -> WeekDate
    toWeek = join (toWeekOrdinal . view ordinalDate)

    {-# INLINEABLE fromWeek #-}
    fromWeek :: WeekDate -> Day
    fromWeek wd@(WeekDate y _ _) = fromWeekLast (lastWeekOfYear y) wd

{-# INLINE toWeekOrdinal #-}
toWeekOrdinal :: OrdinalDate -> Day -> WeekDate
toWeekOrdinal (OrdinalDate y0 yd) (ModifiedJulianDay mjd) =
        WeekDate y1 (w1 + 1) (d7mod + 1) where
    -- pilfered and refactored; no idea what foo and bar mean
    d = mjd + 2
    (d7div, d7mod) = divMod d 7
    foo :: Year -> {-WeekOfYear-1-}Int
    foo y = bar $ ordinalDate # OrdinalDate y 6
    bar :: Day -> {-WeekOfYear-1-}Int
    bar (ModifiedJulianDay k) = d7div - div k 7
    w0 = bar $ ModifiedJulianDay (d - yd + 4)
    (y1, w1) = case w0 of
        -1 -> (y0 - 1, foo (y0 - 1))
        52 | foo (y0 + 1) == 0 -> (y0 + 1, 0)
        _ -> (y0, w0)

{-# INLINE lastWeekOfYear #-}
lastWeekOfYear :: Year -> WeekOfYear
lastWeekOfYear y = if wdWeek wd == 53 then 53 else 52 where
    wd = OrdinalDate y 365 ^. from ordinalDate . weekDate

{-# INLINE fromWeekLast #-}
fromWeekLast :: WeekOfYear -> WeekDate -> Day
fromWeekLast wMax (WeekDate y w d) = ModifiedJulianDay mjd where
    -- pilfered and refactored
    ModifiedJulianDay k = ordinalDate # OrdinalDate y 6
    mjd = k - mod k 7 - 10 + clip 1 7 d + clip 1 wMax w * 7
    clip a b = max a . min b

-- | Convert a 'WeekDate' to a 'Day', or 'Nothing' for invalid 'WeekDate'.
{-# INLINEABLE weekDateValid #-}
weekDateValid :: WeekDate -> Maybe Day
weekDateValid wd@(WeekDate (lastWeekOfYear -> wMax) w d) =
    fromWeekLast wMax wd <$ guard (1 <= d && d <= 7 && 1 <= w && w <= wMax)

-- | Shows a 'Day' using the @yyyy-Www-d@ ISO 8601 Week Date format.
--
-- @
-- > 'showWeekDate' ('gregorian' 'Control.Lens.#' 'YearMonthDay' 2006 11 15)
-- "2006-W46-3"
-- @
{-# INLINEABLE showWeekDate #-}
showWeekDate :: Day -> String
showWeekDate (view weekDate -> WeekDate y w d) =
    showsYear y . (++) "-W" . shows02 w . (:) '-' $ show d

------------------------------------------------------------------------

-- | Week-based calendar date with the first /Sunday/ of the year as the first
-- day of week /01/. This corresponds to @%U@ and @%w@ of
-- @<http://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime strftime(3)>@.
--
-- The final week of a given year and week /00/ of the next both refer to
-- the same week.
data SundayWeek = SundayWeek
    { swYear :: {-# UNPACK #-}!Year
        -- ^ Coincides with that of 'gregorian'.
    , swWeek :: {-# UNPACK #-}!WeekOfYear
        -- ^ Weeks numbered from /00/ to /53/, starting with the first
        -- /Sunday/ of the year as the first day of week /01/.
    , swDay :: {-# UNPACK #-}!DayOfWeek
        -- ^ /0 = Sunday/.
    } deriving (Eq, Ord, Data, Typeable, Generic, Show)

instance Hashable SundayWeek
instance NFData SundayWeek

-- | Conversion between 'Day' and 'SundayWeek'.
--
-- @
-- > 'YearMonthDay' 2016 1 3 '^.' 'from' 'gregorian' '.' 'sundayWeek'
-- 'SundayWeek' {'swYear' = 2016, 'swWeek' = 1, 'swDay' = 0}
-- @
{-# INLINE sundayWeek #-}
sundayWeek :: Iso' Day SundayWeek
sundayWeek = iso toSunday fromSunday where

    {-# INLINEABLE toSunday #-}
    toSunday :: Day -> SundayWeek
    toSunday = join (toSundayOrdinal . view ordinalDate)

    {-# INLINEABLE fromSunday #-}
    fromSunday :: SundayWeek -> Day
    fromSunday (SundayWeek y w d) = ModifiedJulianDay (firstDay + yd) where
        ModifiedJulianDay firstDay = ordinalDate # OrdinalDate y 1
        -- following are all 0-based year days
        firstSunday = mod (4 - firstDay) 7
        yd = firstSunday + 7 * (w - 1) + d

{-# INLINE toSundayOrdinal #-}
toSundayOrdinal :: OrdinalDate -> Day -> SundayWeek
toSundayOrdinal (OrdinalDate y yd) (ModifiedJulianDay mjd) =
        SundayWeek y (d7div - div k 7) d7mod where
    d = mjd + 3
    k = d - yd
    (d7div, d7mod) = divMod d 7

-- | Convert a 'SundayWeek' to a 'Day', or 'Nothing' for invalid 'SundayWeek'.
{-# INLINEABLE sundayWeekValid #-}
sundayWeekValid :: SundayWeek -> Maybe Day
sundayWeekValid (SundayWeek y w d) = ModifiedJulianDay (firstDay + yd)
        <$ guard (0 <= d && d <= 6 && 0 <= yd && yd <= lastDay) where
    ModifiedJulianDay firstDay = ordinalDate # OrdinalDate y 1
    -- following are all 0-based year days
    firstSunday = mod (4 - firstDay) 7
    yd = firstSunday + 7 * (w - 1) + d
    lastDay = if isLeapYear y then 365 else 364

------------------------------------------------------------------------

-- | Week-based calendar date with the first /Monday/ of the year as the first
-- day of week /01/. This corresponds to @%W@ and @%u@ of
-- @<http://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime strftime(3)>@.
--
-- The final week of a given year and week /00/ of the next both refer to
-- the same week.
data MondayWeek = MondayWeek
    { mwYear :: {-# UNPACK #-}!Year
        -- ^ Coincides with that of 'gregorian'.
    , mwWeek :: {-# UNPACK #-}!WeekOfYear
        -- ^ Weeks numbered from /00/ to /53/, starting with the first
        -- /Monday/ of the year as the first day of week /01/.
    , mwDay :: {-# UNPACK #-}!DayOfWeek
        -- ^ /7 = Sunday/.
    } deriving (Eq, Ord, Data, Typeable, Generic, Show)

instance Hashable MondayWeek
instance NFData MondayWeek

-- | Conversion between 'Day' and 'MondayWeek'.
--
-- @
-- > 'YearMonthDay' 2016 1 3 '^.' 'from' 'gregorian' '.' 'mondayWeek'
-- 'MondayWeek' {'mwYear' = 2016, 'mwWeek' = 0, 'mwDay' = 7}
-- @
{-# INLINE mondayWeek #-}
mondayWeek :: Iso' Day MondayWeek
mondayWeek = iso toMonday fromMonday where

    {-# INLINEABLE toMonday #-}
    toMonday :: Day -> MondayWeek
    toMonday = join (toMondayOrdinal . view ordinalDate)

    {-# INLINEABLE fromMonday #-}
    fromMonday :: MondayWeek -> Day
    fromMonday (MondayWeek y w d) = ModifiedJulianDay (firstDay + yd) where
        ModifiedJulianDay firstDay = ordinalDate # OrdinalDate y 1
        -- following are all 0-based year days
        firstMonday = mod (5 - firstDay) 7
        yd = firstMonday + 7 * (w - 1) + d - 1

{-# INLINE toMondayOrdinal #-}
toMondayOrdinal :: OrdinalDate -> Day -> MondayWeek
toMondayOrdinal (OrdinalDate y yd) (ModifiedJulianDay mjd) =
        MondayWeek y (d7div - div k 7) (d7mod + 1) where
    d = mjd + 2
    k = d - yd
    (d7div, d7mod) = divMod d 7

-- | Convert a 'MondayWeek' to a 'Day', or 'Nothing' for invalid 'MondayWeek'.
{-# INLINEABLE mondayWeekValid #-}
mondayWeekValid :: MondayWeek -> Maybe Day
mondayWeekValid (MondayWeek y w d) = ModifiedJulianDay (firstDay + yd)
        <$ guard (1 <= d && d <= 7 && 0 <= yd && yd <= lastDay) where
    ModifiedJulianDay firstDay = ordinalDate # OrdinalDate y 1
    -- following are all 0-based year days
    firstMonday = mod (5 - firstDay) 7
    yd = firstMonday + 7 * (w - 1) + d - 1
    lastDay = if isLeapYear y then 365 else 364

------------------------------------------------------------------------
-- Unbox instances at the end avoids TH-related declaration order issues

derivingUnbox "Day" [t| Day -> Int |]
    [| toModifiedJulianDay |] [| ModifiedJulianDay |]

derivingUnbox "YearMonthDay" [t| YearMonthDay -> Int |]
    [| \ YearMonthDay {..} -> shiftL ymdYear 9 .|. shiftL ymdMonth 5 .|. ymdDay |]
    [| \ n -> YearMonthDay (shiftR n 9) (shiftR n 5 .&. 0xf) (n .&. 0x1f) |]

derivingUnbox "OrdinalDate" [t| OrdinalDate -> Int |]
    [| \ OrdinalDate {..} -> shiftL odYear 9 .|. odDay |]
    [| \ n -> OrdinalDate (shiftR n 9) (n .&. 0x1ff) |]

derivingUnbox "MonthDay" [t| MonthDay -> Int |]
    [| \ MonthDay {..} -> shiftL mdMonth 5 .|. mdDay |]
    [| \ n -> MonthDay (shiftR n 5) (n .&. 0x1f) |]

derivingUnbox "WeekDate" [t| WeekDate -> Int |]
    [| \ WeekDate {..} -> shiftL wdYear 9 .|. shiftL wdWeek 3 .|. wdDay |]
    [| \ n -> WeekDate (shiftR n 9) (shiftR n 3 .&. 0x3f) (n .&. 0x7) |]

derivingUnbox "SundayWeek" [t| SundayWeek -> Int |]
    [| \ SundayWeek {..} -> shiftL swYear 9 .|. shiftL swWeek 3 .|. swDay |]
    [| \ n -> SundayWeek (shiftR n 9) (shiftR n 3 .&. 0x3f) (n .&. 0x7) |]

derivingUnbox "MondayWeek" [t| MondayWeek -> Int |]
    [| \ MondayWeek {..} -> shiftL mwYear 9 .|. shiftL mwWeek 3 .|. mwDay |]
    [| \ n -> MondayWeek (shiftR n 9) (shiftR n 3 .&. 0x3f) (n .&. 0x7) |]

makeLensesFor [("ymdYear","_ymdYear"),("ymdMonth","_ymdMonth"),("ymdDay","_ymdDay")] ''YearMonthDay

makeLensesFor [("odYear","_odYear"),("odDay","_odDay")] ''OrdinalDate

makeLensesFor [("mdMonth","_mdMonth"),("mdDay","_mdDay")] ''MonthDay

makeLensesFor [("wdYear","_wdYear"),("wdWeek","_wdWeek"),("wdDay","_wdDay")] ''WeekDate

makeLensesFor [("swYear","_swYear"),("swWeek","_swWeek"),("swDay","_swDay")] ''SundayWeek

makeLensesFor [("mwYear","_mwYear"),("mwWeek","_mwWeek"),("mwDay","_mwDay")] ''MondayWeek
