{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#include "thyme.h"

-- | Various Week Date formats
module Data.Thyme.Calendar.WeekDate
    ( Year, WeekOfYear, DayOfWeek
    -- * ISO 8601 Week Date
    , WeekDate (..), weekDate, weekDateValid, showWeekDate
    -- * Weeks starting Sunday
    , SundayWeek (..), sundayWeek, sundayWeekValid
    -- * Weeks starting Monday
    , MondayWeek (..), mondayWeek, mondayWeekValid
    , module Data.Thyme.Calendar.WeekDate
    ) where

import Prelude
import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.Internal
import System.Random
import Test.QuickCheck

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

-- * Lenses

LENS(WeekDate,wdYear,Year)
LENS(WeekDate,wdWeek,WeekOfYear)
LENS(WeekDate,wdDay,DayOfWeek)

LENS(SundayWeek,swYear,Year)
LENS(SundayWeek,swWeek,WeekOfYear)
LENS(SundayWeek,swDay,DayOfWeek)

LENS(MondayWeek,mwYear,Year)
LENS(MondayWeek,mwWeek,WeekOfYear)
LENS(MondayWeek,mwDay,DayOfWeek)

