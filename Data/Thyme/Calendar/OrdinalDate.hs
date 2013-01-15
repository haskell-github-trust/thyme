{-# LANGUAGE TemplateHaskell #-}

-- | ISO 8601 Ordinal Date format

module Data.Thyme.Calendar.OrdinalDate
    ( Year, DayOfYear
    , module Data.Thyme.Calendar.OrdinalDate
    ) where

import Prelude
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH

data OrdinalDate = OrdinalDate
    { odYear :: {-# UNPACK #-}!Year
    , odDay :: {-# UNPACK #-}!DayOfYear }

{-# INLINE ordinalDate #-}
ordinalDate :: Simple Iso Day OrdinalDate
ordinalDate = iso toOrd fromOrd where

    {-# INLINEABLE toOrd #-}
    toOrd :: Day -> OrdinalDate
    toOrd (ModifiedJulianDay mjd) = OrdinalDate
            (fromIntegral year) (fromIntegral yd) where
        -- pilfered
        a = mjd + 678575
        quadcent = div a 146097
        b = mod a 146097
        cent = min (div b 36524) 3
        c = b - cent * 36524
        quad = div c 1461
        d = mod c 1461
        y = min (div d 365) 3
        yd = d - y * 365 + 1
        year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

    {-# INLINEABLE fromOrd #-}
    fromOrd :: OrdinalDate -> Day
    fromOrd (OrdinalDate year yd) = ModifiedJulianDay mjd where
        -- pilfered
        y = fromIntegral (year - 1)
        mjd = 365 * y + div y 4 - div y 100 + div y 400 - 678576
            + clip 1 (if isLeapYear year then 366 else 365) (fromIntegral yd)
        clip a b = max a . min b

{-# INLINE fromOrdinalDateValid #-}
fromOrdinalDateValid :: OrdinalDate -> Maybe Day
fromOrdinalDateValid od@(OrdinalDate y d) = review ordinalDate od
    <$ guard (1 <= d && d <= if isLeapYear y then 366 else 365)

{-# INLINE isLeapYear #-}
isLeapYear :: Year -> Bool
isLeapYear y = mod y 4 == 0 && (mod y 400 == 0 || mod y 100 /= 0)

-- TODO: mondayStartWeek fromMondayStartWeek fromMondayStartWeekValid
-- TODO: sundayStartWeek fromSundayStartWeek fromSundayStartWeekValid

-- * Lenses
thymeLenses ''OrdinalDate

