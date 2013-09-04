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
import Control.Applicative
import Control.Lens
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH
import System.Random
import Test.QuickCheck

instance Bounded WeekDate where
    minBound = minBound ^. weekDate
    maxBound = maxBound ^. weekDate

instance Bounded SundayWeek where
    minBound = minBound ^. sundayWeek
    maxBound = maxBound ^. sundayWeek

instance Bounded MondayWeek where
    minBound = minBound ^. mondayWeek
    maxBound = maxBound ^. mondayWeek

instance Random WeekDate where
    randomR = randomIsoR weekDate
    random = over _1 (^. weekDate) . random

instance Random SundayWeek where
    randomR = randomIsoR sundayWeek
    random = over _1 (^. sundayWeek) . random

instance Random MondayWeek where
    randomR = randomIsoR mondayWeek
    random = over _1 (^. mondayWeek) . random

instance Arbitrary WeekDate where
    arbitrary = view weekDate <$> arbitrary

instance Arbitrary SundayWeek where
    arbitrary = view sundayWeek <$> arbitrary

instance Arbitrary MondayWeek where
    arbitrary = view mondayWeek <$> arbitrary

-- * Lenses
thymeLenses ''WeekDate
thymeLenses ''SundayWeek
thymeLenses ''MondayWeek

