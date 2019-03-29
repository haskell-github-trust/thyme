{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "thyme.h"
#if HLINT
#include "cabal_macros.h"
#endif

-- | ISO 8601 Ordinal Date format
module Data.Thyme.Calendar.OrdinalDate
    ( Year, isLeapYear
    , DayOfYear
    , OrdinalDate (..), _odYear, _odDay
    , ordinalDate
    , module Data.Thyme.Calendar.OrdinalDate
    ) where

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Thyme.Calendar
import Data.Thyme.Calendar.Internal
import System.Random
#ifdef QUICKCHECK
import Test.QuickCheck
#endif

instance Bounded OrdinalDate where
    minBound = minBound ^. ordinalDate
    maxBound = maxBound ^. ordinalDate

instance Random OrdinalDate where
    randomR = randomIsoR ordinalDate
    random = first (^. ordinalDate) . random

#ifdef QUICKCHECK
instance Arbitrary OrdinalDate where
    arbitrary = view ordinalDate <$> arbitrary
    shrink od = view ordinalDate <$> shrink (ordinalDate # od)

instance CoArbitrary OrdinalDate where
    coarbitrary (OrdinalDate y d) = coarbitrary y . coarbitrary d
#endif

-- | Convert an 'OrdinalDate' to a 'Day', or 'Nothing' for invalid input.
--
-- @
-- > 'ordinalDateValid' ('OrdinalDate' 2015 365)
-- 'Just' 2015-12-31
--
-- > 'ordinalDateValid' ('OrdinalDate' 2015 366)
-- 'Nothing'
--
-- > 'ordinalDateValid' ('OrdinalDate' 2016 366)
-- 'Just' 2016-12-31
-- @
{-# INLINE ordinalDateValid #-}
ordinalDateValid :: OrdinalDate -> Maybe Day
ordinalDateValid od@(OrdinalDate y d) = ordinalDate # od
    <$ guard (1 <= d && d <= if isLeapYear y then 366 else 365)

-- * Compatibility

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

