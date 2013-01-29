{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- #hide
module Data.Thyme.Calendar.Internal where

import Prelude
import Control.DeepSeq
import Control.Lens
import Data.AffineSpace
import Data.Data
import Data.Int
import Data.Ix

-- | The Modified Julian Day is a standard count of days, with zero being
-- the day 1858-11-17.
newtype Day = ModifiedJulianDay
    { toModifiedJulianDay :: Int64
    } deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable)

instance AffineSpace Day where
    type Diff Day = Int
    {-# INLINE (.-.) #-}
    ModifiedJulianDay a .-. ModifiedJulianDay b = fromIntegral (a - b)
    {-# INLINE (.+^) #-}
    ModifiedJulianDay a .+^ d = ModifiedJulianDay (a + fromIntegral d)

------------------------------------------------------------------------

type Year = Int
type Month = Int
type DayOfMonth = Int

data YearMonthDay = YearMonthDay
    { ymdYear :: {-# UNPACK #-}!Year
    , ymdMonth :: {-# UNPACK #-}!Month
    , ymdDay :: {-# UNPACK #-}!DayOfMonth
    } deriving (Eq, Ord, Data, Typeable, Show)

instance NFData YearMonthDay

------------------------------------------------------------------------

-- | Gregorian leap year?
{-# INLINE isLeapYear #-}
isLeapYear :: Year -> Bool
isLeapYear y = mod y 4 == 0 && (mod y 400 == 0 || mod y 100 /= 0)

type DayOfYear = Int
data OrdinalDate = OrdinalDate
    { odYear :: {-# UNPACK #-}!Year
    , odDay :: {-# UNPACK #-}!DayOfYear
    } deriving (Eq, Ord, Data, Typeable, Show)

instance NFData OrdinalDate

{-# INLINE ordinalDate #-}
ordinalDate :: Simple Iso Day OrdinalDate
ordinalDate = iso toOrd fromOrd where

    {-# INLINEABLE toOrd #-}
    toOrd :: Day -> OrdinalDate
    toOrd (ModifiedJulianDay mjd) = OrdinalDate
            (fromIntegral year) (fromIntegral yd) where
        -- pilfered
        a = mjd + 678575
        quadcent = div a 146097
        b = mod a 146097
        cent = min (div b 36524) 3
        c = b - cent * 36524
        quad = div c 1461
        d = mod c 1461
        y = min (div d 365) 3
        yd = d - y * 365 + 1
        year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

    {-# INLINEABLE fromOrd #-}
    fromOrd :: OrdinalDate -> Day
    fromOrd (OrdinalDate year yd) = ModifiedJulianDay mjd where
        -- pilfered
        y = fromIntegral (year - 1)
        mjd = 365 * y + div y 4 - div y 100 + div y 400 - 678576
            + clip 1 (if isLeapYear year then 366 else 365) (fromIntegral yd)
        clip a b = max a . min b

------------------------------------------------------------------------

type WeekOfYear = Int
type DayOfWeek = Int
data WeekDate = WeekDate
    { wdYear :: {-# UNPACK #-}!Year
    , wdWeek :: {-# UNPACK #-}!WeekOfYear
    , wdDay :: {-# UNPACK #-}!DayOfWeek
    } deriving (Eq, Ord, Data, Typeable, Show)

instance NFData WeekDate

-- | Accepts 0-based 'DayOfWeek' and 'WeekOfYear' when 'review'ing.
{-# INLINE weekDate #-}
weekDate :: Simple Iso Day WeekDate
weekDate = iso toWeek fromWeek where

    {-# INLINEABLE toWeek #-}
    toWeek :: Day -> WeekDate
    toWeek = join (toWeekOrdinal . view ordinalDate)

    {-# INLINEABLE fromWeek #-}
    fromWeek :: WeekDate -> Day
    fromWeek wd@(WeekDate y _ _) = fromWeekMax (lastWeekOfYear y) wd

{-# INLINE toWeekOrdinal #-}
toWeekOrdinal :: OrdinalDate -> Day -> WeekDate
toWeekOrdinal (OrdinalDate y0 yd) (ModifiedJulianDay mjd) = WeekDate y1
        (fromIntegral $ w1 + 1) (fromIntegral $ d7mod + 1) where
    -- pilfered and refactored; no idea what foo and bar mean
    d = mjd + 2
    (d7div, d7mod) = divMod d 7
    foo :: Year -> {-WeekOfYear-1-}Int64
    foo y = bar $ review ordinalDate (OrdinalDate y 6)
    bar :: Day -> {-WeekOfYear-1-}Int64
    bar (ModifiedJulianDay k) = d7div - div k 7
    w0 = bar $ ModifiedJulianDay (d - fromIntegral yd + 4)
    (y1, w1) = case w0 of
        -1 -> (y0 - 1, foo (y0 - 1))
        52 | foo (y0 + 1) == 0 -> (y0 + 1, 0)
        _ -> (y0, w0)

lastWeekOfYear :: Year -> WeekOfYear
lastWeekOfYear y = if wdWeek wd == 53 then 53 else 52 where
    wd = view (from ordinalDate . weekDate) (OrdinalDate y 365)

{-# INLINE fromWeekMax #-}
fromWeekMax :: WeekOfYear -> WeekDate -> Day
fromWeekMax wMax (WeekDate y w d) = ModifiedJulianDay mjd where
    -- pilfered and refactored
    ModifiedJulianDay k = review ordinalDate (OrdinalDate y 6)
    -- FIXME: Is it okay to clip d to 0 in the case of Sunday-starting
    -- weeks, and clip w to 0 for OrdinalDate.{sun,mon}dayStartWeek?
    mjd = k - mod k 7 - 10 + clip 0 7 (fromIntegral d)
        + fromIntegral (clip 0 wMax w) * 7
    clip a b = max a . min b

