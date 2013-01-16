{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Thyme.Format
    ( FormatTime (..)
    , formatTime
    , ParseTime (..)
    , parseTime, readTime, readsTime
    ) where

import Prelude
import Control.Applicative
import Control.Lens
import Data.Char
import Data.Micro
import Data.Thyme.Calendar
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.WeekDate
import Data.Thyme.Clock.POSIX
import Data.Thyme.Clock.Scale
import Data.Thyme.Clock.UTC
import Data.Thyme.LocalTime
import Data.Thyme.Format.Internal
import Data.Time.Format
import System.Locale

{-# INLINE fmtMap #-}
fmtMap :: (s -> a) ->
    (TimeLocale -> Maybe NumericPadOption -> a -> String) ->
    (TimeLocale -> Maybe NumericPadOption -> s -> String)
fmtMap f fmt = \ l mpado -> fmt l mpado . f

instance FormatTime LocalTime where
    {-# INLINEABLE formatCharacter #-}
    formatCharacter c = case c of
        'c' -> Just $ \ l _ -> formatTime l (dateTimeFmt l)
        _ -> fmtMap localDay <$> formatCharacter c
            <|> fmtMap localTimeOfDay <$> formatCharacter c

instance FormatTime TimeOfDay where
    {-# INLINEABLE formatCharacter #-}
    formatCharacter c = case c of
        -- Aggregate
        'R' -> Just $ \ _ _ (TimeOfDay h m _) -> shows02 h . (:) ':' . shows02 m $ ""
        'T' -> Just $ \ _ _ (TimeOfDay h m (DiffTime s)) -> shows02 h . (:) ':' . shows02 m . (:) ':' . shows02 (microTruncate s) $ ""
        'X' -> Just $ \ l _ -> formatTime l (timeFmt l)
        'r' -> Just $ \ l _ -> formatTime l (time12Fmt l)
        -- AM/PM
        'P' -> Just $ \ (amPm -> (a, p)) _ (todHour -> h) -> toLower <$> if h < 12 then a else p
        'p' -> Just $ \ (amPm -> (a, p)) _ (todHour -> h) -> if h < 12 then a else p

        -- Hour
        'H' -> Just $ \ _ _ (todHour -> h) -> show02 h
        'I' -> Just $ \ _ _ (todHour -> h) -> show02 (mod (h - 1) 12 + 1)
        'k' -> Just $ \ _ _ (todHour -> h) -> show_2 h
        'l' -> Just $ \ _ _ (todHour -> h) -> show_2 (mod (h - 1) 12 + 1)
        -- Minute
        'M' -> Just $ \ _ _ (todMin -> m) -> show02 m
        -- Second
        'S' -> Just $ \ _ _ (todSec -> DiffTime s) -> show02 (microTruncate s)
        'q' -> Just $ \ _ _ (todSec -> DiffTime s) -> show60 . snd . microQuotRem s $ Micro 1000000
        'Q' -> Just $ \ _ _ (todSec -> DiffTime s) -> show6 . snd . microQuotRem s $ Micro 1000000

        -- Default
        _ -> Nothing

instance FormatTime ZonedTime where
    {-# INLINEABLE formatCharacter #-}
    formatCharacter c = case c of
        -- need 'c' here again to get the TimeZone
        'c' -> Just $ \ l _ -> formatTime l (dateTimeFmt l)
        's' -> Just $ \ _ _ (view (from zonedTime . _2 . posixTime) -> NominalDiffTime s) -> show (microTruncate s)
        _ -> fmtMap zonedTimeToLocalTime <$> formatCharacter c
            <|> fmtMap zonedTimeZone <$> formatCharacter c

instance FormatTime Day where
    {-# INLINEABLE formatCharacter #-}
    formatCharacter c = case c of
        -- Aggregate
        'D' -> Just $ \ _ _ (view gregorian -> YearMonthDay y m d) ->
            shows02 m . (:) '/' . shows02 d . (:) '/' . shows02 (mod y 100) $ ""
        'F' -> Just $ \ _ _ (view gregorian -> YearMonthDay y m d) ->
            shows04 y . (:) '-' . shows02 m . (:) '-' . shows02 d $ ""
        'x' -> Just $ \ l _ -> formatTime l (dateFmt l)

        -- Year Count
        'Y' -> Just $ \ _ _ -> show04 . ymdYear . view gregorian
        'y' -> Just $ \ _ _ -> show02 . (`mod` 100) . ymdYear . view gregorian
        'C' -> Just $ \ _ _ -> show02 . (`div` 100) . ymdYear . view gregorian
        -- Month of Year
        'B' -> Just $ \ l _ -> fst . (months l !!) . pred . ymdMonth . view gregorian
        'b' -> Just $ \ l _ -> snd . (months l !!) . pred . ymdMonth . view gregorian
        'h' -> Just $ \ l _ -> snd . (months l !!) . pred . ymdMonth . view gregorian
        'm' -> Just $ \ _ _ -> show02 . ymdMonth . view gregorian
        -- Day of Month
        'd' -> Just $ \ _ _ -> show02 . ymdDay . view gregorian
        'e' -> Just $ \ _ _ -> show_2 . ymdDay . view gregorian
        -- Day of Year
        'j' -> Just $ \ _ _ -> show03 . odDay . view ordinalDate

        -- Non-standard Day of week
        'A' -> Just $ \ l _ -> fst . (wDays l !!) . wdDay . sundayStartWeek
        'a' -> Just $ \ l _ -> snd . (wDays l !!) . wdDay . sundayStartWeek
        'U' -> Just $ \ _ _ -> show02 . wdWeek . sundayStartWeek
        'W' -> Just $ \ _ _ -> show02 . wdWeek . mondayStartWeek
        'w' -> Just $ \ _ _ -> show . wdDay . sundayStartWeek

        -- ISO 8601 Week Date
        'G' -> Just $ \ _ _ -> show04 . wdYear . view weekDate
        'g' -> Just $ \ _ _ -> show02 . (`mod` 100) . wdYear . view weekDate
        'f' -> Just $ \ _ _ -> show02 . (`div` 100) . wdYear . view weekDate

        'V' -> Just $ \ _ _ -> show02 . wdWeek . view weekDate
        'u' -> Just $ \ _ _ -> show . wdDay . view weekDate

        -- Default
        _ -> Nothing

instance FormatTime UTCTime where
    {-# INLINEABLE formatCharacter #-}
    formatCharacter c = fmtMap (view zonedTime . (,) utc) <$> formatCharacter c

