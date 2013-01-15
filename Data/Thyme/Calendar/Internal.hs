{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- #hide
module Data.Thyme.Calendar.Internal where

import Prelude
import Control.DeepSeq
import Data.AffineSpace
import Data.Data
import Data.Int
import Data.Ix

-- | The Modified Julian Day is a standard count of days, with zero being
-- the day 1858-11-17.
newtype Day = ModifiedJulianDay
    { toModifiedJulianDay :: Int64
    } deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable)

#if 1 /*SHOW_INTERNAL*/
deriving instance Show Day
#endif

instance AffineSpace Day where
    type Diff Day = Int
    {-# INLINE (.-.) #-}
    ModifiedJulianDay a .-. ModifiedJulianDay b = fromIntegral (a - b)
    {-# INLINE (.+^) #-}
    ModifiedJulianDay a .+^ d = ModifiedJulianDay (a + fromIntegral d)

------------------------------------------------------------------------

type Year = Int
type DayOfYear = Int
type Month = Int
type DayOfMonth = Int

data YearMonthDay = YearMonthDay
    { ymdYear :: {-# UNPACK #-}!Year
    , ymdMonth :: {-# UNPACK #-}!Month
    , ymdDay :: {-# UNPACK #-}!DayOfMonth
    } deriving (Eq, Ord, Data, Typeable, Show)

instance NFData YearMonthDay

------------------------------------------------------------------------

type Week = Int
type DayOfWeek = Int
data WeekDate = WeekDate
    { wdYear :: {-# UNPACK #-}!Year
    , wdWeek :: {-# UNPACK #-}!Week
    , wdDay :: {-# UNPACK #-}!DayOfWeek
    } deriving (Eq, Ord, Data, Typeable, Show)

