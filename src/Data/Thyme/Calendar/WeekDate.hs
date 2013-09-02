{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Various Week Date formats
module Data.Thyme.Calendar.WeekDate
    ( Year, WeekOfYear, DayOfWeek
    -- * ISO 8601 Week Date
    , WeekDate (..), weekDate, weekDateValid, showWeekDate
    -- * Weeks starting Sunday
    , SundayWeek (..), sundayWeek, sundayWeekValid
    -- * Weeks starting Monday
    , MondayWeek (..), mondayWeek, mondayWeekValid
    , module Data.Thyme.Calendar.WeekDate
    ) where

import Prelude
import Control.Lens
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH

instance Bounded WeekDate where
    minBound = minBound ^. weekDate
    maxBound = maxBound ^. weekDate

instance Bounded SundayWeek where
    minBound = minBound ^. sundayWeek
    maxBound = maxBound ^. sundayWeek

instance Bounded MondayWeek where
    minBound = minBound ^. mondayWeek
    maxBound = maxBound ^. mondayWeek

-- * Lenses
thymeLenses ''WeekDate
thymeLenses ''SundayWeek
thymeLenses ''MondayWeek

