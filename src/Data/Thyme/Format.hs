{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Thyme.Format
    ( FormatTime (..)
    , formatTime
    ) where

import Prelude
import Control.Applicative
import Control.Lens
import Data.Char
import Data.Micro
import Data.Thyme.Calendar
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.WeekDate
import Data.Thyme.Calendar.MonthDay
import Data.Thyme.Clock.POSIX
import Data.Thyme.Clock.Scale
import Data.Thyme.Clock.UTC
import Data.Thyme.LocalTime
import Data.Thyme.Format.Internal
import System.Locale

type FormatS = Char -> ShowS
class FormatTime t where
    showsTime :: TimeLocale -> t -> FormatS -> FormatS

{-# INLINEABLE formatTime #-}
formatTime :: (FormatTime t) => TimeLocale -> String -> t -> String
formatTime l@TimeLocale {..} spec t = go spec "" where
    -- leave unrecognised codes as they are
    format = showsTime l t (\ c s -> '%' : c : s)
    go s = case s of
        '%' : c : rest -> case c of
            -- aggregate
            'c' -> go (dateTimeFmt ++ rest)
            'r' -> go (time12Fmt ++ rest)
            'X' -> go (timeFmt ++ rest)
            'x' -> go (dateFmt ++ rest)
            -- modifier (whatever)
            '-' -> go ('%' : rest)
            '_' -> go ('%' : rest)
            '0' -> go ('%' : rest)
            '^' -> go ('%' : rest)
            '#' -> go ('%' : rest)
            -- escape (why would anyone need %t and %n?)
            '%' -> (:) '%' . go rest
            -- default
            _ -> format c . go rest
        c : rest -> (:) c . go rest
        [] -> id

{-# INLINE showsYear #-}
showsYear :: Year -> ShowS
#if BUG_FOR_BUG
showsYear = shows
#else
-- ISO 8601 says 4 digits, even for first millennium.
showsYear = shows04
#endif

instance FormatTime TimeOfDay where
    {-# INLINEABLE showsTime #-}
    showsTime TimeLocale {..} (TimeOfDay h m (DiffTime s)) = \ def c -> case c of
        -- aggregate
        'R' -> shows02 h . (:) ':' . shows02 m
        'T' -> shows02 h . (:) ':' . shows02 m . (:) ':' . shows02 si
        -- AM/PM
        'P' -> (++) $ toLower <$> if h < 12 then fst amPm else snd amPm
        'p' -> (++) $ if h < 12 then fst amPm else snd amPm
        -- Hour
        'H' -> shows02 h
        'I' -> shows02 $ 1 + mod (h - 1) 12
        'k' -> shows_2 h
        'l' -> shows_2 $ 1 + mod (h - 1) 12
        -- Minute
        'M' -> shows02 m
        -- Second
        'S' -> shows02 si
        'q' -> fills06 su . shows su . (++) "000000"
        'Q' -> if su == 0 then id else (:) '.' . fills06 su . drops0 su
        -- default
        _ -> def c
        where (fromIntegral -> si, Micro su) = microQuotRem s (Micro 1000000)

instance FormatTime YearMonthDay where
    {-# INLINEABLE showsTime #-}
    showsTime TimeLocale {..} (YearMonthDay y m d) = \ def c -> case c of
        -- aggregate
        'D' -> shows02 m . (:) '/' . shows02 d . (:) '/' . shows02 (mod y 100)
        'F' -> showsYear y . (:) '-' . shows02 m . (:) '-' . shows02 d
        -- Year
        'Y' -> showsYear y
        'y' -> shows02 (mod y 100)
        'C' -> shows02 (div y 100)
        -- Month
        'B' -> (++) . fst $ months !! (m - 1)
        'b' -> (++) . snd $ months !! (m - 1)
        'h' -> (++) . snd $ months !! (m - 1)
        'm' -> shows02 m
        -- DayOfMonth
        'd' -> shows02 d
        'e' -> shows_2 d
        -- default
        _ -> def c

instance FormatTime MonthDay where
    {-# INLINEABLE showsTime #-}
    showsTime TimeLocale {..} (MonthDay m d) = \ def c -> case c of
        -- Month
        'B' -> (++) . fst $ months !! (m - 1)
        'b' -> (++) . snd $ months !! (m - 1)
        'h' -> (++) . snd $ months !! (m - 1)
        'm' -> shows02 m
        -- DayOfMonth
        'd' -> shows02 d
        'e' -> shows_2 d
        -- default
        _ -> def c

instance FormatTime OrdinalDate where
    {-# INLINEABLE showsTime #-}
    showsTime TimeLocale {..} (OrdinalDate y d) = \ def c -> case c of
        -- Year
        'Y' -> showsYear y
        'y' -> shows02 (mod y 100)
        'C' -> shows02 (div y 100)
        -- DayOfYear
        'j' -> shows03 d
        -- default
        _ -> def c

instance FormatTime WeekDate where
    {-# INLINEABLE showsTime #-}
    showsTime TimeLocale {..} (WeekDate y w d) = \ def c -> case c of
        -- Year
        'G' -> showsYear y
        'g' -> shows02 (mod y 100)
        'f' -> shows02 (div y 100)
        -- WeekOfYear
        'V' -> shows02 w
        -- DayOfWeek
        'u' -> shows $ if d == 0 then 7 else d
        'w' -> shows $ if d == 7 then 0 else d
        'A' -> (++) . fst $ wDays !! mod d 7
        'a' -> (++) . snd $ wDays !! mod d 7
        -- default
        _ -> def c

instance FormatTime SundayWeek where
    {-# INLINEABLE showsTime #-}
    showsTime TimeLocale {..} (SundayWeek y w d) = \ def c -> case c of
        -- Year
        'Y' -> showsYear y
        'y' -> shows02 (mod y 100)
        'C' -> shows02 (div y 100)
        -- WeekOfYear
        'U' -> shows02 w
        -- DayOfWeek
        'u' -> shows $ if d == 0 then 7 else d
        'w' -> shows $ if d == 7 then 0 else d
        'A' -> (++) . fst $ wDays !! mod d 7
        'a' -> (++) . snd $ wDays !! mod d 7
        -- default
        _ -> def c

instance FormatTime MondayWeek where
    {-# INLINEABLE showsTime #-}
    showsTime TimeLocale {..} (MondayWeek y w d) = \ def c -> case c of
        -- Year
        'Y' -> showsYear y
        'y' -> shows02 (mod y 100)
        'C' -> shows02 (div y 100)
        -- WeekOfYear
        'W' -> shows02 w
        -- DayOfWeek
        'u' -> shows $ if d == 0 then 7 else d
        'w' -> shows $ if d == 7 then 0 else d
        'A' -> (++) . fst $ wDays !! mod d 7
        'a' -> (++) . snd $ wDays !! mod d 7
        -- default
        _ -> def c

instance FormatTime LocalTime where
    {-# INLINEABLE showsTime #-}
    showsTime l (LocalTime day tod) = showsTime l day . showsTime l tod

instance FormatTime Day where
    {-# INLINEABLE showsTime #-}
    showsTime l d = showsTime l ordinal
            . showsTime l (view yearMonthDay ordinal)
            . showsTime l (view weekDate d)
            . showsTime l (view sundayWeek d)
            . showsTime l (view mondayWeek d) where
        -- FIXME: could be a little more efficient wrt week dates here
        ordinal = view ordinalDate d

instance FormatTime TimeZone where
    {-# INLINEABLE showsTime #-}
    showsTime _ tz@(TimeZone _ _ name) = \ def c -> case c of
        'z' -> (++) (timeZoneOffsetString tz)
        'Z' -> (++) (if null name then timeZoneOffsetString tz else name)
        _ -> def c

instance FormatTime ZonedTime where
    {-# INLINEABLE showsTime #-}
    showsTime l (ZonedTime lt tz) = showsTime l lt . showsTime l tz

instance FormatTime UTCTime where
    {-# INLINEABLE showsTime #-}
    showsTime l t = \ def c -> case c of
        's' -> shows . fst $ qr s (Micro 1000000)
        _ -> showsTime l (view zonedTime (utc, t)) def c
      where
        NominalDiffTime s = view posixTime t
#if BUG_FOR_BUG
        qr = microDivMod -- rounds down
#else
        qr = microQuotRem -- rounds to 0
#endif

