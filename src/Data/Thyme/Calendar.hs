{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Thyme.Calendar
    ( Years, Months, Days
    -- * Days
    , Day (..)
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
import Data.Thyme.Format.Internal
import Data.Thyme.TH

{-# INLINE yearMonthDay #-}
yearMonthDay :: Iso' OrdinalDate YearMonthDay
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
gregorian :: Iso' Day YearMonthDay
gregorian = ordinalDate . yearMonthDay

{-# INLINEABLE gregorianValid #-}
gregorianValid :: YearMonthDay -> Maybe Day
gregorianValid (YearMonthDay y m d) = review ordinalDate . OrdinalDate y
    <$> monthDayValid (isLeapYear y) (MonthDay m d)

{-# INLINEABLE showGregorian #-}
showGregorian :: Day -> String
showGregorian (view gregorian -> YearMonthDay y m d) =
    showsYear y . (:) '-' . shows02 m . (:) '-' . shows02 d $ ""

#if SHOW_INTERNAL
deriving instance Show Day
#else
instance Show Day where show = showGregorian
#endif

{-# INLINE gregorianMonthLength #-}
gregorianMonthLength :: Year -> Month -> Days
gregorianMonthLength = monthLength . isLeapYear

{-# INLINEABLE gregorianMonthsClip #-}
gregorianMonthsClip :: Months -> YearMonthDay -> YearMonthDay
gregorianMonthsClip n (YearMonthDay y m d) = YearMonthDay y' m'
        $ min (gregorianMonthLength y' m') d where
    ((+) y -> y', (+) 1 -> m') = divMod (m + n - 1) 12

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
thymeLenses ''Day
thymeLenses ''YearMonthDay

