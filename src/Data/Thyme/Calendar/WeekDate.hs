{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | ISO 8601 Week Date format
module Data.Thyme.Calendar.WeekDate
    ( Year, WeekOfYear, DayOfWeek
    , WeekDate (..), weekDate
    , module Data.Thyme.Calendar.WeekDate
    ) where

import Prelude
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH
import Text.Printf

-- | Rejects 0-based 'DayOfWeek' and 'WeekOfYear'.
{-# INLINEABLE fromWeekDateValid #-}
fromWeekDateValid :: WeekDate -> Maybe Day
fromWeekDateValid wd@(WeekDate (lastWeekOfYear -> wMax) w d) =
    fromWeekMax wMax wd <$ guard (1 <= d && d <= 7 && 1 <= w && w <= wMax)

{-# INLINEABLE showWeekDate #-}
showWeekDate :: Day -> String
showWeekDate (view weekDate -> WeekDate y w d) = printf "%04d-W%02d-%d" y w d

-- * Lenses
thymeLenses ''WeekDate

