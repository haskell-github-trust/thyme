{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | ISO 8601 Ordinal Date format

module Data.Thyme.Calendar.OrdinalDate
    ( Year, isLeapYear
    , DayOfYear, OrdinalDate (..), ordinalDate
    , module Data.Thyme.Calendar.OrdinalDate
    ) where

import Prelude
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH

{-# INLINE fromOrdinalDateValid #-}
fromOrdinalDateValid :: OrdinalDate -> Maybe Day
fromOrdinalDateValid od@(OrdinalDate y d) = review ordinalDate od
    <$ guard (1 <= d && d <= if isLeapYear y then 366 else 365)

-- | Use @'review' 'weekDate'@ to convert back to 'Day'.
{-# INLINE sundayStartWeek #-}
sundayStartWeek :: Day -> WeekDate
sundayStartWeek day@(ModifiedJulianDay mjd) = WeekDate y
        (fromIntegral $ div d 7 - div k 7) (fromIntegral $ mod d 7) where
    OrdinalDate y yd = view ordinalDate day
    d = mjd + 3
    k = d - fromIntegral yd

-- | Accepts 0−6 for 'DayOfWeek', and 0-based 'WeekOfYear's.
{-# INLINEABLE fromSundayStartWeekValid #-}
fromSundayStartWeekValid :: WeekDate -> Maybe Day
fromSundayStartWeekValid wd@(WeekDate (lastWeekOfYear -> wMax) w d) =
    fromWeekMax wMax wd <$ guard (0 <= d && d <= 6 && 0 <= w && w <= wMax)

-- | Use @'review' 'weekDate'@ to convert back to 'Day'.
{-# INLINE mondayStartWeek #-}
mondayStartWeek :: Day -> WeekDate
mondayStartWeek day@(ModifiedJulianDay mjd) = WeekDate y
        (fromIntegral $ div d 7 - div k 7) (fromIntegral $ mod d 7 + 1) where
    OrdinalDate y yd = view ordinalDate day
    d = mjd + 2
    k = d - fromIntegral yd

-- | Accepts 1−7 for 'DayOfWeek', and 0-based 'WeekOfYear's.
{-# INLINEABLE fromMondayStartWeekValid #-}
fromMondayStartWeekValid :: WeekDate -> Maybe Day
fromMondayStartWeekValid wd@(WeekDate (lastWeekOfYear -> wMax) w d) =
    fromWeekMax wMax wd <$ guard (1 <= d && d <= 7 && 0 <= w && w <= wMax)

-- * Lenses
thymeLenses ''OrdinalDate

