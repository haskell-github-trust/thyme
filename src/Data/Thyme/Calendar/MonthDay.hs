{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.Time.Calendar.MonthDay as T
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH

data MonthDay = MonthDay
    { mdMonth :: {-# UNPACK #-}!Month
    , mdDay :: {-# UNPACK #-}!DayOfMonth
    } deriving (Eq, Ord, Data, Typeable, Show)

instance NFData MonthDay

-- | Convert between day of year in the Gregorian or Julian calendars, and
-- month and day of month. First arg is leap year flag.
{-# INLINE monthDay #-}
monthDay :: Bool -> Simple Iso DayOfYear MonthDay
monthDay leap = iso fromOrdinal toOrdinal where
    -- TODO: Calls non-inlineable code from @time@. Pilfer and optimise?

    {-# INLINE fromOrdinal #-}
    fromOrdinal :: DayOfYear -> MonthDay
    fromOrdinal yd = (MonthDay m d) where
        (m, d) = T.dayOfYearToMonthAndDay leap yd

    {-# INLINE toOrdinal #-}
    toOrdinal :: MonthDay -> DayOfYear
    toOrdinal (MonthDay m d) = T.monthAndDayToDayOfYear leap m d

{-# INLINEABLE monthDayToDayOfYearValid #-}
monthDayToDayOfYearValid :: Bool -> MonthDay -> Maybe DayOfYear
monthDayToDayOfYearValid leap md@(MonthDay m d) = review (monthDay leap) md
    <$ guard (1 <= m && m <= 12 && 1 <= d && d <= T.monthLength leap m)

{-# INLINE monthLength #-}
monthLength :: Bool -> Month -> Int
monthLength = T.monthLength

-- * Lenses
thymeLenses ''MonthDay

