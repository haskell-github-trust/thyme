{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ == 706
{-# OPTIONS_GHC -fsimpl-tick-factor=120 #-} -- 7.6.3 only, it seems; fixes #29
#endif

#include "thyme.h"
#if HLINT
#include "cabal_macros.h"
#endif

-- | Various Week Date formats
module Data.Thyme.Calendar.WeekDate
    ( Year, WeekOfYear, DayOfWeek
    -- * ISO 8601 Week Date
    , WeekDate (..), _wdYear, _wdWeek, _wdDay
    , weekDate, weekDateValid, showWeekDate

    -- * Weeks starting Sunday
    , SundayWeek (..), _swYear, _swWeek, _swDay
    , sundayWeek, sundayWeekValid

    -- * Weeks starting Monday
    , MondayWeek (..), _mwYear, _mwWeek, _mwDay
    , mondayWeek, mondayWeekValid

    , module Data.Thyme.Calendar.WeekDate
    ) where

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Arrow
import Control.Lens
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.Internal
import System.Random
#ifdef QUICKCHECK
import Test.QuickCheck
#endif

instance Bounded WeekDate where
    minBound = minBound ^. weekDate
    maxBound = maxBound ^. weekDate

instance Bounded SundayWeek where
    minBound = minBound ^. sundayWeek
    maxBound = maxBound ^. sundayWeek

instance Bounded MondayWeek where
    minBound = minBound ^. mondayWeek
    maxBound = maxBound ^. mondayWeek

instance Random WeekDate where
    randomR = randomIsoR weekDate
    random = first (^. weekDate) . random

instance Random SundayWeek where
    randomR = randomIsoR sundayWeek
    random = first (^. sundayWeek) . random

instance Random MondayWeek where
    randomR = randomIsoR mondayWeek
    random = first (^. mondayWeek) . random

#ifdef QUICKCHECK
instance Arbitrary WeekDate where
    arbitrary = view weekDate <$> arbitrary
    shrink wd = view weekDate <$> shrink (weekDate # wd)

instance Arbitrary SundayWeek where
    arbitrary = view sundayWeek <$> arbitrary
    shrink sw = view sundayWeek <$> shrink (sundayWeek # sw)

instance Arbitrary MondayWeek where
    arbitrary = view mondayWeek <$> arbitrary
    shrink mw = view mondayWeek <$> shrink (mondayWeek # mw)

instance CoArbitrary WeekDate where
    coarbitrary (WeekDate y w d)
        = coarbitrary y . coarbitrary w . coarbitrary d

instance CoArbitrary SundayWeek where
    coarbitrary (SundayWeek y w d)
        = coarbitrary y . coarbitrary w . coarbitrary d

instance CoArbitrary MondayWeek where
    coarbitrary (MondayWeek y w d)
        = coarbitrary y . coarbitrary w . coarbitrary d
#endif

-- * Compatibility

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

