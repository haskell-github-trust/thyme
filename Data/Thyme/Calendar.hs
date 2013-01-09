module Data.Thyme.Calendar (
    -- * Days
      Day (..)
    -- * Gregorian calendar
    , Year, Month, DayOfMonth
    , YearMonthDay (..)
    , module Data.Thyme.Calendar
    , isLeapYear
    -- * Lenses
    , _toModifiedJulianDay
    , _ymdYear, _ymdMonth, _ymdDay
    ) where

import Prelude hiding ((.))
import Control.Applicative
import Control.Category
import Control.Lens
import Data.Thyme.Calendar.Day
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.MonthDay

{-# INLINE gregorian #-}
gregorian :: Simple Iso Day YearMonthDay
gregorian = ordinalDate . iso fromOrdinal toOrdinal where

    {-# INLINE fromOrdinal #-}
    fromOrdinal :: OrdinalDate -> YearMonthDay
    fromOrdinal (OrdinalDate y yd) = (YearMonthDay y m d) where
        MonthDay m d = view (monthDay (isLeapYear y)) yd

    {-# INLINE toOrdinal #-}
    toOrdinal :: YearMonthDay -> OrdinalDate
    toOrdinal (YearMonthDay y m d) = OrdinalDate y $
        review (monthDay (isLeapYear y)) (MonthDay m d)

{-# INLINE fromGregorianValid #-}
fromGregorianValid :: YearMonthDay -> Maybe Day
fromGregorianValid (YearMonthDay y m d) = review ordinalDate . OrdinalDate y
    <$> monthDayToDayOfYearValid (isLeapYear y) (MonthDay m d)

-- TODO: showGregorian

{-# INLINE gregorianMonthLength #-}
gregorianMonthLength :: Year -> Month -> Int
gregorianMonthLength = monthLength . isLeapYear

-- TODO: addGregorianMonthsClip addGregorianMonthsRollover
-- TODO: addGregorianYearsClip addGregorianYearsRollover

