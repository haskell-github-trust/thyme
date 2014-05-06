{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
#include "thyme.h"

module Data.Thyme.Calendar.WeekdayOfMonth
    ( Year, Month, DayOfWeek
    , module Data.Thyme.Calendar.WeekdayOfMonth
    ) where

import Prelude
import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Data
import Data.Thyme.Calendar
import Data.Thyme.Calendar.Internal
import GHC.Generics (Generic)
import System.Random
import Test.QuickCheck

data WeekdayOfMonth = WeekdayOfMonth
    { womYear :: {-# UNPACK #-}!Year
    , womMonth :: {-# UNPACK #-}!Month
    , womNth :: {-# UNPACK #-}!Int -- ^ ±1–5, negative means n-th last
    , womDayOfWeek :: {-# UNPACK #-}!DayOfWeek
    } deriving (INSTANCES_USUAL, Show)

instance NFData WeekdayOfMonth

instance Bounded WeekdayOfMonth where
    minBound = minBound ^. weekdayOfMonth
    maxBound = maxBound ^. weekdayOfMonth

instance Random WeekdayOfMonth where
    randomR = randomIsoR weekdayOfMonth
    random = first (^. weekdayOfMonth) . random

instance Arbitrary WeekdayOfMonth where
    arbitrary = view weekdayOfMonth <$> arbitrary
    shrink wom = view weekdayOfMonth <$> shrink (weekdayOfMonth # wom)

instance CoArbitrary WeekdayOfMonth where
    coarbitrary (WeekdayOfMonth y m n d)
        = coarbitrary y . coarbitrary m
        . coarbitrary n . coarbitrary d

{-# INLINE weekdayOfMonth #-}
weekdayOfMonth :: Iso' Day WeekdayOfMonth
weekdayOfMonth = iso toWeekday fromWeekday where

    {-# INLINEABLE toWeekday #-}
    toWeekday :: Day -> WeekdayOfMonth
    toWeekday day@(view ordinalDate -> ord) = WeekdayOfMonth y m n wd where
        YearMonthDay y m d = ord ^. yearMonthDay
        WeekDate _ _ wd = toWeekOrdinal ord day
        n = 1 + div (d - 1) 7

    {-# INLINEABLE fromWeekday #-}
    fromWeekday :: WeekdayOfMonth -> Day
    fromWeekday (WeekdayOfMonth y m n wd) = refDay .+^ s * offset where
        refOrd = yearMonthDay # YearMonthDay y m
            (if n < 0 then monthLength (isLeapYear y) m else 1)
        refDay = ordinalDate # refOrd
        WeekDate _ _ wd1 = toWeekOrdinal refOrd refDay
        s = signum n
        wo = s * (wd - wd1)
        offset = (abs n - 1) * 7 + if wo < 0 then wo + 7 else wo

{-# INLINEABLE weekdayOfMonthValid #-}
weekdayOfMonthValid :: WeekdayOfMonth -> Maybe Day
weekdayOfMonthValid (WeekdayOfMonth y m n wd) = (refDay .+^ s * offset)
        <$ guard (n /= 0 && 1 <= wd && wd <= 7 && offset < len) where
    len = monthLength (isLeapYear y) m
    refOrd = yearMonthDay # YearMonthDay y m (if n < 0 then len else 1)
    refDay = ordinalDate # refOrd
    WeekDate _ _ wd1 = toWeekOrdinal refOrd refDay
    s = signum n
    wo = s * (wd - wd1)
    offset = (abs n - 1) * 7 + if wo < 0 then wo + 7 else wo

-- * Lenses
LENS(WeekdayOfMonth,womYear,Year)
LENS(WeekdayOfMonth,womMonth,Month)
LENS(WeekdayOfMonth,womNth,Int)
LENS(WeekdayOfMonth,womDayOfWeek,DayOfWeek)

