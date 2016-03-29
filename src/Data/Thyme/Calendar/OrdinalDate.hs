{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "thyme.h"
#if HLINT
#include "cabal_macros.h"
#endif

-- | ISO 8601 Ordinal Date format
module Data.Thyme.Calendar.OrdinalDate
    ( Year, isLeapYear
    , DayOfYear, OrdinalDate (..), ordinalDate
    , module Data.Thyme.Calendar.OrdinalDate
    ) where

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
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

instance CoArbitrary OrdinalDate where
    coarbitrary (OrdinalDate y d) = coarbitrary y . coarbitrary d

-- | Is this a valid ordinal date?
--
-- ==== Examples
--
-- @
-- > ordinalDateValid ('OrdinalDate' 2015 365)
--   Just 2015-12-31
-- @
--
-- @
-- > ordinalDateValid ('OrdinalDate' 2015 366)
--   Nothing
-- @
--
-- @
-- > ordinalDateValid ('OrdinalDate' 2016 366)
--   Just 2016-12-31
-- @
{-# INLINE ordinalDateValid #-}
ordinalDateValid :: OrdinalDate -> Maybe Day
ordinalDateValid od@(OrdinalDate y d) = ordinalDate # od
    <$ guard (1 <= d && d <= if isLeapYear y then 366 else 365)

-- * Lenses
LENS(OrdinalDate,odYear,Year)
LENS(OrdinalDate,odDay,DayOfYear)

