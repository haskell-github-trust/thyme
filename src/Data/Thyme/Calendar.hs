{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Thyme.Calendar (
    -- * Days
      Day (..)
    -- * Gregorian calendar
    , Year, Month, DayOfMonth
    , YearMonthDay (..)
    , isLeapYear
    , module Data.Thyme.Calendar
    ) where

import Prelude hiding ((.))
import Control.Applicative
import Control.Category
import Control.Lens
import Data.Thyme.Calendar.Internal
import Data.Thyme.Calendar.MonthDay
import Data.Thyme.Format.Internal
import Data.Thyme.TH

{-# INLINE yearMonthDay #-}
yearMonthDay :: Simple Iso OrdinalDate YearMonthDay
yearMonthDay = iso fromOrdinal toOrdinal where

    {-# INLINEABLE fromOrdinal #-}
    fromOrdinal :: OrdinalDate -> YearMonthDay
    fromOrdinal (OrdinalDate y yd) = YearMonthDay y m d where
        MonthDay m d = view (monthDay (isLeapYear y)) yd

    {-# INLINEABLE toOrdinal #-}
    toOrdinal :: YearMonthDay -> OrdinalDate
    toOrdinal (YearMonthDay y m d) = OrdinalDate y $
        review (monthDay (isLeapYear y)) (MonthDay m d)

{-# INLINE gregorian #-}
gregorian :: Simple Iso Day YearMonthDay
gregorian = ordinalDate . yearMonthDay

{-# INLINEABLE gregorianValid #-}
gregorianValid :: YearMonthDay -> Maybe Day
gregorianValid (YearMonthDay y m d) = review ordinalDate . OrdinalDate y
    <$> monthDayValid (isLeapYear y) (MonthDay m d)

{-# INLINEABLE showGregorian #-}
showGregorian :: Day -> String
showGregorian (view gregorian -> YearMonthDay y m d) =
    shows04 y . (:) '-' . shows02 m . (:) '-' . shows02 d $ ""

#if SHOW_INTERNAL
deriving instance Show Day
#else
instance Show Day where show = showGregorian
#endif

{-# INLINE gregorianMonthLength #-}
gregorianMonthLength :: Year -> Month -> Int
gregorianMonthLength = monthLength . isLeapYear

-- TODO: addGregorianMonthsClip addGregorianMonthsRollover
-- TODO: addGregorianYearsClip addGregorianYearsRollover

-- * Lenses
thymeLenses ''Day
thymeLenses ''YearMonthDay

