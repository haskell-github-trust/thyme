{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "thyme.h"
#if HLINT
#include "cabal_macros.h"
#endif

-- | Calendar calculations.
--
-- Note that 'UTCTime' is not Y294K-compliant, and 'Bounded' instances for
-- the various calendar types reflect this fact. That said, the calendar
-- calculations by themselves work perfectly fine for a wider range of
-- dates, subject to the size of 'Int' for your platform.
module Data.Thyme.Calendar
    (
    -- * Day
      Day (..), modifiedJulianDay

    -- * Calendar
    , Year, Month, DayOfMonth
    , YearMonthDay (..), _ymdYear, _ymdMonth, _ymdDay
    , Years, Months, Days

    -- * Gregorian calendar
    -- $proleptic
    , isLeapYear
    , yearMonthDay, gregorian, gregorianValid, showGregorian
    , module Data.Thyme.Calendar
    ) where

import Prelude hiding ((.))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Arrow
import Control.Category
import Control.Lens
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Thyme.Calendar.Internal
import Data.Thyme.Clock.Internal
import System.Random
#ifdef QUICKCHECK
import Control.Monad
import Test.QuickCheck
#endif

-- "Data.Thyme.Calendar.Internal" cannot import "Data.Thyme.Clock.Internal",
-- therefore these orphan 'Bounded' instances must live here.
instance Bounded Day where
    minBound = minBound ^. _utctDay
    maxBound = maxBound ^. _utctDay

instance Bounded YearMonthDay where
    minBound = minBound ^. gregorian
    maxBound = maxBound ^. gregorian

instance Random Day where
    randomR r = first (^. _utctDay) . randomR (range r) where
        -- upper bound is one Micro second before the next day
        range = toMidnight *** pred . toMidnight . succ
        toMidnight day = utcTime # UTCView day zeroV
    random = randomR (minBound, maxBound)

instance Random YearMonthDay where
    randomR = randomIsoR gregorian
    random = first (^. gregorian) . random

#ifdef QUICKCHECK
instance Arbitrary Day where
    arbitrary = ModifiedJulianDay
        <$> choose (join (***) toModifiedJulianDay (minBound, maxBound))
    shrink (ModifiedJulianDay mjd) = ModifiedJulianDay <$> shrink mjd

instance Arbitrary YearMonthDay where
    arbitrary = view gregorian <$> arbitrary
    shrink ymd = view gregorian <$> shrink (gregorian # ymd)

instance CoArbitrary YearMonthDay where
    coarbitrary (YearMonthDay y m d)
        = coarbitrary y . coarbitrary m . coarbitrary d
#endif

------------------------------------------------------------------------

-- $proleptic
--
-- Note that using the
-- <https://en.wikipedia.org/wiki/Gregorian_calendar Gregorian> calendar for
-- dates before its adoption (from 1582 onwards, but varies from one country
-- to the next) produces
-- <https://en.wikipedia.org/wiki/Gregorian_calendar#Proleptic_Gregorian_calendar a proleptic calendar>,
-- which may cause some confusion.

-- | The number of days in a given month in the
-- <https://en.wikipedia.org/wiki/Gregorian_calendar Gregorian> calendar.
--
-- @
-- > 'gregorianMonthLength' 2005 2
-- 28
-- @
{-# INLINE gregorianMonthLength #-}
gregorianMonthLength :: Year -> Month -> Days
gregorianMonthLength = monthLength . isLeapYear

-- | Add months, with days past the last day of the month clipped to the
-- last day.
--
-- See also 'addGregorianMonthsClip'.
--
-- @
-- > 'gregorianMonthsClip' 1 '$' 'YearMonthDay' 2005 1 30
-- 'YearMonthDay' {'ymdYear' = 2005, 'ymdMonth' = 2, 'ymdDay' = 28}
-- @
{-# INLINEABLE gregorianMonthsClip #-}
gregorianMonthsClip :: Months -> YearMonthDay -> YearMonthDay
gregorianMonthsClip n (YearMonthDay y m d) = YearMonthDay y' m'
        $ min (gregorianMonthLength y' m') d where
    ((+) y -> y', (+) 1 -> m') = divMod (m + n - 1) 12

-- | Add months, with days past the last day of the month rolling over to
-- the next month.
--
-- See also 'addGregorianMonthsRollover'.
--
-- @
-- > 'gregorianMonthsRollover' 1 $ 'YearMonthDay' 2005 1 30
-- 'YearMonthDay' {'ymdYear' = 2005, 'ymdMonth' = 3, 'ymdDay' = 2}
-- @
{-# ANN gregorianMonthsRollover "HLint: ignore Use if" #-}
{-# INLINEABLE gregorianMonthsRollover #-}
gregorianMonthsRollover :: Months -> YearMonthDay -> YearMonthDay
gregorianMonthsRollover n (YearMonthDay y m d) = case d <= len of
    True -> YearMonthDay y' m' d
    False -> case m' < 12 of
        True -> YearMonthDay y' (m' + 1) (d - len)
        False -> YearMonthDay (y' + 1) 1 (d - len)
  where
    ((+) y -> y', (+) 1 -> m') = divMod (m + n - 1) 12
    len = gregorianMonthLength y' m'

-- | Add years, matching month and day, with /February 29th/ clipped to the
-- /28th/ if necessary.
--
-- See also 'addGregorianYearsClip'.
--
-- @
-- > 'gregorianYearsClip' 2 $ 'YearMonthDay' 2004 2 29
-- 'YearMonthDay' {'ymdYear' = 2006, 'ymdMonth' = 2, 'ymdDay' = 28}
-- @
{-# INLINEABLE gregorianYearsClip #-}
gregorianYearsClip :: Years -> YearMonthDay -> YearMonthDay
gregorianYearsClip n (YearMonthDay ((+) n -> y') 2 29)
    | not (isLeapYear y') = YearMonthDay y' 2 28
gregorianYearsClip n (YearMonthDay y m d) = YearMonthDay (y + n) m d

-- | Add years, matching month and day, with /February 29th/ rolled over to
-- /March 1st/ if necessary.
--
-- See also 'addGregorianYearsRollover'.
--
-- @
-- > 'gregorianYearsRollover' 2 $ 'YearMonthDay' 2004 2 29
-- 'YearMonthDay' {'ymdYear' = 2006, 'ymdMonth' = 3, 'ymdDay' = 1}
-- @
{-# INLINEABLE gregorianYearsRollover #-}
gregorianYearsRollover :: Years -> YearMonthDay -> YearMonthDay
gregorianYearsRollover n (YearMonthDay ((+) n -> y') 2 29)
    | not (isLeapYear y') = YearMonthDay y' 3 1
gregorianYearsRollover n (YearMonthDay y m d) = YearMonthDay (y + n) m d

-- * Compatibility

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

