{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Various Week Date formats
module Data.Thyme.Calendar.WeekDate
    ( Year, WeekOfYear, DayOfWeek
    -- * ISO 8601 Week Date
    , WeekDate (..), weekDate, weekDateValid, showWeekDate
    , SundayWeek (..), sundayWeek, sundayWeekValid
    , MondayWeek (..), mondayWeek, mondayWeekValid
    , module Data.Thyme.Calendar.WeekDate
    ) where

import Prelude
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH

-- * Lenses
thymeLenses ''WeekDate
thymeLenses ''SundayWeek
thymeLenses ''MondayWeek

