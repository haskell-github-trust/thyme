{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
#include "thyme.h"

-- | Calendar months and day-of-months.
module Data.Thyme.Calendar.MonthDay
    ( Month, DayOfMonth, MonthDay (..)
    , monthDay, monthDayValid, monthLength
    , module Data.Thyme.Calendar.MonthDay
    ) where

import Prelude
import Control.Lens
import Data.Thyme.Calendar.Internal

-- * Lenses
LENS(MonthDay,mdMonth,Month)
LENS(MonthDay,mdDay,DayOfMonth)

