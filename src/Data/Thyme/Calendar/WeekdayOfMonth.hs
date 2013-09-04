{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Thyme.Calendar.WeekdayOfMonth where

import Prelude
import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Data
import Data.Thyme.Calendar
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH
import System.Random
import Test.QuickCheck

data WeekdayOfMonth = WeekdayOfMonth
    { womYear :: {-# UNPACK #-}!Year
    , womMonth :: {-# UNPACK #-}!Month
    , womNth :: {-# UNPACK #-}!Int -- ^ ±1–5, negative means n-th last
    , womDayOfWeek :: {-# UNPACK #-}!DayOfWeek
    } deriving (Eq, Ord, Data, Typeable, Show)

instance NFData WeekdayOfMonth

instance Bounded WeekdayOfMonth where
    minBound = minBound ^. weekdayOfMonth
    maxBound = maxBound ^. weekdayOfMonth

instance Random WeekdayOfMonth where
    randomR = randomIsoR weekdayOfMonth
    random = over _1 (^. weekdayOfMonth) . random

instance Arbitrary WeekdayOfMonth where
    arbitrary = view weekdayOfMonth <$> arbitrary

{-# INLINE weekdayOfMonth #-}
weekdayOfMonth :: Iso' Day WeekdayOfMonth
weekdayOfMonth = iso toWeekday fromWeekday where

    {-# INLINEABLE toWeekday #-}
    toWeekday :: Day -> WeekdayOfMonth
    toWeekday day@(view ordinalDate -> ord) = WeekdayOfMonth y m n wd where
        YearMonthDay y m d = ord ^. yearMonthDay
        WeekDate _ _ wd = toWeekOrdinal ord day
        n = div (d - 1) 7

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
thymeLenses ''WeekdayOfMonth

