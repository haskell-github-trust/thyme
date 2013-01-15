{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- | ISO 8601 Week Date format
module Data.Thyme.Calendar.WeekDate
    ( WeekDate (..)
    , module Data.Thyme.Calendar.WeekDate
    ) where

import Prelude
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Int
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.Internal
import Data.Thyme.TH
import Text.Printf

{-# INLINE weekDate #-}
weekDate :: Simple Iso Day WeekDate
weekDate = iso toWeek fromWeek where

    {-# INLINEABLE toWeek #-}
    toWeek :: Day -> WeekDate
    toWeek day@(ModifiedJulianDay mjd) = WeekDate
            y1 (fromIntegral $ w1 + 1) (fromIntegral $ mod d 7 + 1) where
        -- pilfered and refactored; no idea what foo and bar mean
        OrdinalDate y0 yd = view ordinalDate day
        d = mjd + 2
        foo :: Year -> Int64
        foo y = bar $ review ordinalDate (OrdinalDate y 6)
        bar :: Day -> Int64
        bar (ModifiedJulianDay k) = div d 7 - div k 7
        w0 = bar $ ModifiedJulianDay (d - fromIntegral yd + 4)
        (y1, w1) = case w0 of
            -1 -> (y0 - 1, foo (y0 - 1))
            52 | foo (y0 + 1) == 0 -> (y0 + 1, 0)
            _ -> (y0, w0)

    {-# INLINEABLE fromWeek #-}
    fromWeek :: WeekDate -> Day
    fromWeek (WeekDate y w wd) = ModifiedJulianDay mjd where
        -- pilfered and refactored
        ModifiedJulianDay k = review ordinalDate (OrdinalDate y 6)
        WeekDate _ wMax _ = toWeek $ review ordinalDate (OrdinalDate y 365)
        mjd = k - mod k 7 - 10 + clip 1 7 (fromIntegral wd)
            + fromIntegral (clip 1 wMax w) * 7
        clip a b = max a . min b

fromWeekDateValid :: WeekDate -> Maybe Day
fromWeekDateValid wd@(WeekDate y w d) = review weekDate wd
        <$ guard (1 <= d && d <= 7 && 1 <= w && w <= wMax) where
    -- TODO: inline fromWeek so we can share wMax?
    WeekDate _ wMax _ = view (from ordinalDate . weekDate) (OrdinalDate y 365)

{-# INLINEABLE showWeekDate #-}
showWeekDate :: Day -> String
showWeekDate day = printf "%04d-W%02d-%d" y w d where
    WeekDate y w d = view weekDate day

-- * Lenses
thymeLenses ''WeekDate

