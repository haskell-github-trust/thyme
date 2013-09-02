{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | ISO 8601 Ordinal Date format

module Data.Thyme.Calendar.OrdinalDate
    ( Year, isLeapYear
    , DayOfYear, OrdinalDate (..), ordinalDate
    , module Data.Thyme.Calendar.OrdinalDate
    ) where

import Prelude
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Thyme.Calendar
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH

instance Bounded OrdinalDate where
    minBound = minBound ^. ordinalDate
    maxBound = maxBound ^. ordinalDate

{-# INLINE ordinalDateValid #-}
ordinalDateValid :: OrdinalDate -> Maybe Day
ordinalDateValid od@(OrdinalDate y d) = review ordinalDate od
    <$ guard (1 <= d && d <= if isLeapYear y then 366 else 365)

-- * Lenses
thymeLenses ''OrdinalDate

