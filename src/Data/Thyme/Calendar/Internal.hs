{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

#include "thyme.h"

module Data.Thyme.Calendar.Internal where

import Prelude
import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Bits
import Data.Data
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

type Years = Int
type Months = Int
type Days = Int

-- | The Modified Julian Day is a standard count of days, with zero being
-- the day 1858-11-17.
newtype Day = ModifiedJulianDay
    { toModifiedJulianDay :: Int
    } deriving (INSTANCES_NEWTYPE, CoArbitrary)

instance AffineSpace Day where
    type Diff Day = Days
    {-# INLINE (.-.) #-}
    (.-.) = \ (ModifiedJulianDay a) (ModifiedJulianDay b) -> a - b
    {-# INLINE (.+^) #-}
    (.+^) = \ (ModifiedJulianDay a) d -> ModifiedJulianDay (a + d)

{-# INLINE modifiedJulianDay #-}
modifiedJulianDay :: Iso' Day Int
modifiedJulianDay = iso toModifiedJulianDay ModifiedJulianDay

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

{-# INLINE gregorian #-}
gregorian :: Iso' Day YearMonthDay
gregorian = ordinalDate . yearMonthDay

{-# INLINEABLE gregorianValid #-}
gregorianValid :: YearMonthDay -> Maybe Day
gregorianValid (YearMonthDay y m d) = review ordinalDate . OrdinalDate y
    <$> monthDayValid (isLeapYear y) (MonthDay m d)

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

type Year = Int
type Month = Int
type DayOfMonth = Int

data YearMonthDay = YearMonthDay
    { ymdYear :: {-# UNPACK #-}!Year
    , ymdMonth :: {-# UNPACK #-}!Month
    , ymdDay :: {-# UNPACK #-}!DayOfMonth
    } deriving (INSTANCES_USUAL, Show)

instance NFData YearMonthDay

------------------------------------------------------------------------

-- | Gregorian leap year?
isLeapYear :: Year -> Bool
isLeapYear y = y .&. 3 == 0 && (r100 /= 0 || q100 .&. 3 == 0) where
    (q100, r100) = y `quotRem` 100

type DayOfYear = Int
data OrdinalDate = OrdinalDate
    { odYear :: {-# UNPACK #-}!Year
    , odDay :: {-# UNPACK #-}!DayOfYear
    } deriving (INSTANCES_USUAL, Show)

instance NFData OrdinalDate

{-# INLINE ordinalDate #-}
ordinalDate :: Iso' Day OrdinalDate
ordinalDate = iso toOrd fromOrd where

    {-# INLINEABLE toOrd #-}
    toOrd :: Day -> OrdinalDate
    toOrd (ModifiedJulianDay mjd) = OrdinalDate year yd where
        -- pilfered
        a = mjd + 678575
        (quadcent, b) = divMod a 146097
        cent = min (div b 36524) 3
        c = b - cent * 36524
        (quad, d) = divMod c 1461
        y = min (div d 365) 3
        yd = d - y * 365 + 1
        year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

    {-# INLINEABLE fromOrd #-}
    fromOrd :: OrdinalDate -> Day
    fromOrd (OrdinalDate year yd) = ModifiedJulianDay mjd where
        -- pilfered
        y = year - 1
        mjd = 365 * y + div y 4 - div y 100 + div y 400 - 678576
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

data MonthDay = MonthDay
    { mdMonth :: {-# UNPACK #-}!Month
    , mdDay :: {-# UNPACK #-}!DayOfMonth
    } deriving (INSTANCES_USUAL, Show)

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

-- | Convert between day of year in the Gregorian or Julian calendars, and
-- month and day of month. First arg is leap year flag.
{-# INLINE monthDay #-}
monthDay :: Bool -> Iso' DayOfYear MonthDay
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

{-# INLINEABLE monthDayValid #-}
monthDayValid :: Bool -> MonthDay -> Maybe DayOfYear
monthDayValid leap md@(MonthDay m d) = monthDay leap # md
    <$ guard (1 <= m && m <= 12 && 1 <= d && d <= monthLength leap m)

{-# INLINEABLE monthLength #-}
monthLength :: Bool -> Month -> Days
monthLength leap = VU.unsafeIndex ls . max 0 . min 11 . pred where
    ls = if leap then monthLengthsLeap else monthLengths

------------------------------------------------------------------------

type WeekOfYear = Int
type DayOfWeek = Int

-- | Weeks numbered 01 to 53, where week 01 is the first week that has at
-- least 4 days in the new year. Days before week 01 are considered to
-- belong to the previous year.
data WeekDate = WeekDate
    { wdYear :: {-# UNPACK #-}!Year
    , wdWeek :: {-# UNPACK #-}!WeekOfYear
    , wdDay :: {-# UNPACK #-}!DayOfWeek
    } deriving (INSTANCES_USUAL, Show)

instance NFData WeekDate

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

{-# INLINEABLE weekDateValid #-}
weekDateValid :: WeekDate -> Maybe Day
weekDateValid wd@(WeekDate (lastWeekOfYear -> wMax) w d) =
    fromWeekLast wMax wd <$ guard (1 <= d && d <= 7 && 1 <= w && w <= wMax)

{-# INLINEABLE showWeekDate #-}
showWeekDate :: Day -> String
showWeekDate (view weekDate -> WeekDate y w d) =
    showsYear y . (++) "-W" . shows02 w . (:) '-' . shows d $ ""

------------------------------------------------------------------------

-- | Weeks numbered from 0 to 53, starting with the first Sunday of the year
-- as the first day of week 1. The last week of a given year and week 0 of
-- the next both refer to the same week, but not all 'DayOfWeek' are valid.
-- 'Year' coincides with that of 'gregorian'.
data SundayWeek = SundayWeek
    { swYear :: {-# UNPACK #-}!Year
    , swWeek :: {-# UNPACK #-}!WeekOfYear
    , swDay :: {-# UNPACK #-}!DayOfWeek
    } deriving (INSTANCES_USUAL, Show)

instance NFData SundayWeek

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

-- | Weeks numbered from 0 to 53, starting with the first Monday of the year
-- as the first day of week 1. The last week of a given year and week 0 of
-- the next both refer to the same week, but not all 'DayOfWeek' are valid.
-- 'Year' coincides with that of 'gregorian'.
data MondayWeek = MondayWeek
    { mwYear :: {-# UNPACK #-}!Year
    , mwWeek :: {-# UNPACK #-}!WeekOfYear
    , mwDay :: {-# UNPACK #-}!DayOfWeek
    } deriving (INSTANCES_USUAL, Show)

instance NFData MondayWeek

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

