{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

#if HLINT
#include "cabal_macros.h"
#endif

-- | Calendar date reckoned by year, month-of-year, and n-th day-of-week.
module Data.Thyme.Calendar.WeekdayOfMonth
    ( Year, Month, DayOfWeek
    , module Data.Thyme.Calendar.WeekdayOfMonth
    ) where

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Bits
import Data.Data
import Data.Hashable
import Data.Thyme.Calendar
import Data.Thyme.Calendar.Internal
#if __GLASGOW_HASKELL__ == 704
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
#endif
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)
import System.Random
import Test.QuickCheck hiding ((.&.))

-- | Calendar date with year, month-of-year, and n-th day-of-week.
data WeekdayOfMonth = WeekdayOfMonth
    { womYear :: {-# UNPACK #-}!Year
        -- ^ Calendar year.
    , womMonth :: {-# UNPACK #-}!Month
        -- ^ Month of year.
    , womNth :: {-# UNPACK #-}!Int
        -- ^ /N/-th 'DayOfWeek'. Range /±1–5/; negative means the /N/-th
        -- last 'DayOfWeek' of the month.
    , womDayOfWeek :: {-# UNPACK #-}!DayOfWeek
        -- ^ Day of week. /1 = Monday, 7 = Sunday/, like ISO 8601 'WeekDate'.
    } deriving (Eq, Ord, Data, Typeable, Generic, Show)

makeLensesFor [("womYear","_womYear"),("womMonth","_womMonth"),("womNth","_womNth"),("womDayOfWeek","_womDayOfWeek")] ''WeekdayOfMonth
{-# INLINE _womYear #-}
{-# INLINE _womMonth #-}
{-# INLINE _womNth #-}
{-# INLINE _womDayOfWeek #-}

derivingUnbox "WeekdayOfMonth"
    [t| WeekdayOfMonth -> Int |]
    [| \ WeekdayOfMonth {..} -> shiftL womYear 11 .|. shiftL womMonth 7
        .|. shiftL (womNth + 5) 3 .|. womDayOfWeek |]
    [| \ n -> WeekdayOfMonth (shiftR n 11) (shiftR n 7 .&. 0xf)
        (shiftR n 3 - 5) (n .&. 0x7) |]

instance Hashable WeekdayOfMonth
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

-- | Conversion between a 'Day' and and 'WeekdayOfMonth'.
--
-- This is a proper 'Iso' if and only if all of the 'WeekdayOfMonth' fields
-- are valid and positive.
--
-- For example, the last /Monday/ in /January 2016/ is also the fourth
-- /Monday/:
--
-- @
-- > 'weekdayOfMonth' 'Control.Lens.#' 'WeekdayOfMonth' 2016 1 (-1) 1
-- 2016-01-25
-- > 'YearMonthDay' 2016 01 25 '^.' 'from' 'gregorian' '.' 'weekdayOfMonth'
-- 'WeekdayOfMonth' {'womYear' = 2016, 'womMonth' = 1, 'womNth' = 4, 'womDayOfWeek' = 1}
-- @
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

-- | Convert a 'WeekdayOfMonth' to a 'Day'.
-- Returns 'Nothing' for invalid input.
--
-- For example, the third /Sunday/ of /January 2016/ is /2016-01-27/, but
-- there is no fifth /Monday/ in /January 2016/.
--
-- @
-- > 'weekdayOfMonthValid' ('WeekdayOfMonth' 2016 1 3 7)
-- 'Just' 2016-01-17
-- > 'weekdayOfMonthValid' ('WeekdayOfMonth' 2016 1 5 1)
-- 'Nothing'
-- @
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
