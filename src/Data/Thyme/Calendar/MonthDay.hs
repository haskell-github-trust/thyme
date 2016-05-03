{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

#include "thyme.h"

-- | Calendar months and day-of-months.
module Data.Thyme.Calendar.MonthDay
    ( Month, DayOfMonth, MonthDay (..)
    , monthDay, monthDayValid, monthLength
    , module Data.Thyme.Calendar.MonthDay
    ) where

import Prelude
import Control.Lens
import Data.Thyme.Calendar.Internal

-- * Compatibility

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

-- * Lenses

LENS(MonthDay,mdMonth,Month)
LENS(MonthDay,mdDay,DayOfMonth)

