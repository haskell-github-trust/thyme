{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#include "thyme.h"

-- | ISO 8601 Ordinal Date format
module Data.Thyme.Calendar.OrdinalDate
    ( Year, isLeapYear
    , DayOfYear, OrdinalDate (..), ordinalDate
    , module Data.Thyme.Calendar.OrdinalDate
    ) where

import Prelude
import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Thyme.Calendar
import Data.Thyme.Calendar.Internal
import System.Random
import Test.QuickCheck

instance Bounded OrdinalDate where
    minBound = minBound ^. ordinalDate
    maxBound = maxBound ^. ordinalDate

instance Random OrdinalDate where
    randomR = randomIsoR ordinalDate
    random = first (^. ordinalDate) . random

instance Arbitrary OrdinalDate where
    arbitrary = view ordinalDate <$> arbitrary
    shrink od = view ordinalDate <$> shrink (ordinalDate # od)

{-# INLINE ordinalDateValid #-}
ordinalDateValid :: OrdinalDate -> Maybe Day
ordinalDateValid od@(OrdinalDate y d) = ordinalDate # od
    <$ guard (1 <= d && d <= if isLeapYear y then 366 else 365)

-- * Lenses
LENS(OrdinalDate,odYear,Year)
LENS(OrdinalDate,odDay,DayOfYear)

