{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "thyme.h"
#if HLINT
#include "cabal_macros.h"
#endif

-- | 'UTCTime' is not Y294K-compliant, and 'Bounded' instances for the
-- various calendar types reflect this fact. That said, the calendar
-- calculations by themselves work perfectly fine for a wider range of
-- dates, subject to the size of 'Int' for your platform.
module Data.Thyme.Calendar
    ( Years, Months, Days
    -- * Days
    , Day (..), modifiedJulianDay
    -- * Gregorian calendar
    , Year, Month, DayOfMonth
    , YearMonthDay (..)
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
import Control.Monad
import Data.AdditiveGroup
import Data.Thyme.Calendar.Internal
import Data.Thyme.Clock.Internal
import System.Random
import Test.QuickCheck

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
        toMidnight = (utcTime #) . flip UTCTime zeroV
    random = randomR (minBound, maxBound)

instance Random YearMonthDay where
    randomR = randomIsoR gregorian
    random = first (^. gregorian) . random

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

------------------------------------------------------------------------

{-# INLINE gregorianMonthLength #-}
gregorianMonthLength :: Year -> Month -> Days
gregorianMonthLength = monthLength . isLeapYear

{-# INLINEABLE gregorianMonthsClip #-}
gregorianMonthsClip :: Months -> YearMonthDay -> YearMonthDay
gregorianMonthsClip n (YearMonthDay y m d) = YearMonthDay y' m'
        $ min (gregorianMonthLength y' m') d where
    ((+) y -> y', (+) 1 -> m') = divMod (m + n - 1) 12

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

{-# INLINEABLE gregorianYearsClip #-}
gregorianYearsClip :: Years -> YearMonthDay -> YearMonthDay
gregorianYearsClip n (YearMonthDay ((+) n -> y') 2 29)
    | not (isLeapYear y') = YearMonthDay y' 2 28
gregorianYearsClip n (YearMonthDay y m d) = YearMonthDay (y + n) m d

{-# INLINEABLE gregorianYearsRollover #-}
gregorianYearsRollover :: Years -> YearMonthDay -> YearMonthDay
gregorianYearsRollover n (YearMonthDay ((+) n -> y') 2 29)
    | not (isLeapYear y') = YearMonthDay y' 3 1
gregorianYearsRollover n (YearMonthDay y m d) = YearMonthDay (y + n) m d

-- * Lenses
LENS(YearMonthDay,ymdYear,Year)
LENS(YearMonthDay,ymdMonth,Month)
LENS(YearMonthDay,ymdDay,DayOfMonth)

