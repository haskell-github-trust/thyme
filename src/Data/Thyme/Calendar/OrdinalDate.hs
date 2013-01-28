{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH

{-# INLINE fromOrdinalDateValid #-}
fromOrdinalDateValid :: OrdinalDate -> Maybe Day
fromOrdinalDateValid od@(OrdinalDate y d) = review ordinalDate od
    <$ guard (1 <= d && d <= if isLeapYear y then 366 else 365)

-- * Lenses
thymeLenses ''OrdinalDate

