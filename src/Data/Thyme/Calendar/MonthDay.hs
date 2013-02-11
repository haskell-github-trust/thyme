{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Julian or Gregorian.
module Data.Thyme.Calendar.MonthDay
    ( Month, DayOfMonth
    , module Data.Thyme.Calendar.MonthDay
    ) where

import Prelude
import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Data
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH
import qualified Data.Vector.Unboxed as V

data MonthDay = MonthDay
    { mdMonth :: {-# UNPACK #-}!Month
    , mdDay :: {-# UNPACK #-}!DayOfMonth
    } deriving (Eq, Ord, Data, Typeable, Show)

instance NFData MonthDay

-- | Convert between day of year in the Gregorian or Julian calendars, and
-- month and day of month. First arg is leap year flag.
{-# INLINE monthDay #-}
monthDay :: Bool -> Iso' DayOfYear MonthDay
monthDay leap = iso fromOrdinal toOrdinal where
    (lastDay, lengths, table, ok) = if leap
        then (365, monthLengthsLeap, monthDaysLeap, -1)
        else (364, monthLengths, monthDays, -2)

    {-# INLINE fromOrdinal #-}
    fromOrdinal :: DayOfYear -> MonthDay
    fromOrdinal (max 0 . min lastDay . pred -> i) = MonthDay m d where
        (fromIntegral -> m, fromIntegral -> d) = V.unsafeIndex table i

    {-# INLINE toOrdinal #-}
    toOrdinal :: MonthDay -> DayOfYear
    toOrdinal (MonthDay month day) = div (367 * m - 362) 12 + k + d where
        m = max 1 . min 12 $ month
        l = V.unsafeIndex lengths (pred m)
        d = max 1 . min l $ day
        k = if m <= 2 then 0 else ok

{-# INLINEABLE monthDayValid #-}
monthDayValid :: Bool -> MonthDay -> Maybe DayOfYear
monthDayValid leap md@(MonthDay m d) = review (monthDay leap) md
    <$ guard (1 <= m && m <= 12 && 1 <= d && d <= monthLength leap m)

{-# INLINEABLE monthLength #-}
monthLength :: Bool -> Month -> Days
monthLength leap = V.unsafeIndex ls . max 0 . min 11 . pred where
    ls = if leap then monthLengthsLeap else monthLengths

-- * Lenses
thymeLenses ''MonthDay

