{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
#endif

#include "thyme.h"

-- | Formatting and parsing for dates and times.
module Data.Thyme.Format
    (
    -- * Formatting Date/Time to String
      FormatTime (..)
    , formatTime
    -- * Parsing Date/Time from String
    , ParseTime (..)
    , parseTime
    , readTime
    , readsTime
    , TimeParse (..)
    , timeParser

    -- * Time Locale
    , TimeLocale (..)
    , defaultTimeLocale
    ) where

import Prelude
import Control.Applicative
#if SHOW_INTERNAL
import Control.Arrow
#endif
import Control.Lens
import Control.Monad.Trans
import Control.Monad.State.Strict
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Bits
import qualified Data.ByteString.Char8 as S
import Data.Char
import Data.Int
import Data.Thyme.Internal.Micro
import Data.Thyme.Calendar
import Data.Thyme.Calendar.Internal
import Data.Thyme.Clock.Internal
import Data.Thyme.Clock.POSIX
import Data.Thyme.Clock.TAI
import Data.Thyme.Format.Internal
import Data.Thyme.LocalTime
import Data.VectorSpace

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (TimeLocale (..), defaultTimeLocale)
#else
import System.Locale
#endif

-- | All instances of this class may be formatted by 'formatTime'.
class FormatTime t where
    showsTime
        :: TimeLocale
        -> t
        -> (Char -> ShowS)
        -> Char
        -> ShowS


-- TODO Here are the formatting codes from the @time@ library which are not
--      implemented.
--
-- [@%t@] tab
--
-- [@%n@] newline
--
-- === glibc-style modifiers can be used before the letter (here marked as @z@)
--
-- [@%-z@] no padding
--
-- [@%_z@] pad with spaces
--
-- [@%0z@] pad with zeros
--
-- [@%^z@] convert to upper case
--
-- [@%#z@] convert to lower case (consistently, unlike glibc)
--
-- === For 'UTCTime' and 'ZonedTime'
-- [@%s@] doesn't work for 'ZonedTime'
--
-- [@%Y@] Note __@%0Y@__ and __@%_Y@__ pad to four chars
--
-- [@%G@] Note __@%0G@__ and __@%_G@__ pad to four chars
--
-- [@%f@] Note __@%0f@__ and __@%_f@__ pad to two chars
--
-- [@%C@] Note __@%0C@__ and __@%_C@__ pad to two chars
--
--
-- TODO Format codes for week-of-month?
--
-- TODO It's impossible to print out an ordinal day of month or day of week
--      without padding. Implement the @time@ padding modifiers? Of course
--      the work-around, the 'Show' instance for 'Int', isn't too bad.
--
-- TODO drop the dependency on @old-locale@ for the TimeLocale type?



-- | Format a 'FormatTime' instance value according to a template string.
--
-- These formatting template codes are intended to be compatible with
-- __@glibc@__ @<http://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime strftime()>@
-- function, following "Data.Time.Format", which
-- follows 'System.Time.formatCalendarTime' from the @old-time@ package.
-- Codes which differ from @strftime()@ are marked as
-- /EXTENSION/.
--
-- == Show/Parse template string spec
--
-- === For all types
--
-- [@%%@] literal @\"%\"@
--
-- === For 'TimeZone' (and 'ZonedTime' and 'UTCTime'):
--
-- [@%z@] RFC 822/ISO 8601:1988 style numeric time zone (e.g., @\"-0600\"@ or @\"+0100\"@)
--
-- [@%N@] ISO 8601 style numeric time zone (e.g., @\"-06:00\"@ or @\"+01:00\"@) /EXTENSION/
--
-- [@%Z@] timezone name
--
-- === For 'LocalTime' (and 'ZonedTime' and 'UTCTime' and 'UniversalTime')
--
-- [@%c@] The preferred calendar time representation for the current locale. As 'dateTimeFmt' @locale@ (e.g. __@%a %b %e %H:%M:%S %Z %Y@__)
--
-- === For 'TimeOfDay' (and 'LocalTime' and 'ZonedTime' and 'UTCTime' and 'UniversalTime')
--
-- [@%R@] same as __@%H:%M@__
--
-- [@%T@] same as __@%H:%M:%S@__
--
-- [@%X@] The preferred time of day representation for the current locale. As 'timeFmt' @locale@ (e.g. __@%H:%M:%S@__)
--
-- [@%r@] The complete calendar time using the AM/PM format of the current locale. As 'time12Fmt' @locale@ (e.g. __@%I:%M:%S %p@__)
--
-- [@%P@] day-half of day from ('amPm' @locale@), converted to lowercase, @\"am\"@, @\"pm\"@
--
-- [@%p@] day-half of day from ('amPm' @locale@), @\"AM\"@, @\"PM\"@
--
-- [@%H@] hour of day (24-hour), 0-padded to two chars, @\"00\"@–@\"23\"@
--
-- [@%k@] hour of day (24-hour), space-padded to two chars, @\" 0\"@–@\"23\"@
--
-- [@%I@] hour of day-half (12-hour), 0-padded to two chars, @\"01\"@–@\"12\"@
--
-- [@%l@] hour of day-half (12-hour), space-padded to two chars, @\" 1\"@–@\"12\"@
--
-- [@%M@] minute of hour, 0-padded to two chars, @\"00\"@–@\"59\"@
--
-- [@%S@] second of minute (without decimal part), 0-padded to two chars, @\"00\"@–@\"60\"@
--
-- [@%q@] picosecond of second, 0-padded to twelve chars, @\"000000000000\"@–@\"999999999999\"@. /EXTENSION/
--
-- [@%v@] microsecond of second, 0-padded to six chars, @\"000000\"@–@\"999999\"@. /EXTENSION/
--
-- [@%Q@] decimal point and fraction of second, up to 6 second decimals, without trailing zeros.
--        For a whole number of seconds, __@%Q@__ produces the empty string. /EXTENSION/
--
-- === For 'UTCTime'
--
-- [@%s@] number of whole seconds since the Unix epoch. For times before
-- the Unix epoch, this is a negative number. Note that in __@%s.%q@__ and __@%s%Q@__
-- the decimals are positive, not negative. For example, 0.9 seconds
-- before the Unix epoch is formatted as @\"-1.1\"@ with __@%s%Q@__.
--
-- === For 'Day' (and 'LocalTime' and 'ZonedTime' and 'UTCTime' and 'UniversalTime')
--
-- [@%D@] same as __@%m\/%d\/%y@__
--
-- [@%F@] same as __@%Y-%m-%d@__
--
-- [@%x@] as 'dateFmt' @locale@ (e.g. __@%m\/%d\/%y@__)
--
-- [@%Y@] year, no padding.
--
-- [@%y@] year of century, 0-padded to two chars, @\"00\"@–@\"99\"@
--
-- [@%C@] century, no padding.
--
-- [@%B@] month name, long form ('fst' from 'months' @locale@), @\"January\"@–@\"December\"@
--
-- [@%b@, @%h@] month name, short form ('snd' from 'months' @locale@), @\"Jan\"@–@\"Dec\"@
--
-- [@%m@] month of year, 0-padded to two chars, @\"01\"@–@\"12\"@
--
-- [@%d@] day of month, 0-padded to two chars, @\"01\"@–@\"31\"@
--
-- [@%e@] day of month, space-padded to two chars, @\" 1\"@–@\"31\"@
--
-- [@%j@] day of year, 0-padded to three chars, @\"001\"@–@\"366\"@
--
-- [@%G@] year for Week Date format, no padding.
--
-- [@%g@] year of century for Week Date format, 0-padded to two chars, @\"00\"@–@\"99\"@
--
-- [@%f@] century for Week Date format, no padding. /EXTENSION/
--
-- [@%V@] week of year for Week Date format, 0-padded to two chars, @\"01\"@–@\"53\"@
--
-- [@%u@] day of week for Week Date format, @\"1\"@–@\"7\"@
--
-- [@%a@] day of week, short form ('snd' from 'wDays' @locale@), @\"Sun\"@–@\"Sat\"@
--
-- [@%A@] day of week, long form ('fst' from 'wDays' @locale@), @\"Sunday\"@–@\"Saturday\"@
--
-- [@%U@] week of year where weeks start on Sunday (as 'sundayStartWeek'), 0-padded to two chars, @\"00\"@–@\"53\"@
--
-- [@%w@] day of week number, @\"0\"@ (= Sunday) – @\"6\"@ (= Saturday)
--
-- [@%W@] week of year where weeks start on Monday (as 'Data.Thyme.Calendar.WeekdayOfMonth.mondayStartWeek'), 0-padded to two chars, @\"00\"@–@\"53\"@
--
-- == Examples
--
-- ==== <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
-- @
-- > 'formatTime' 'defaultTimeLocale' "%Y-%m-%dT%H:%M:%S%N" $ 'mkUTCTime' 2015 1 15  12 34 56.78
-- "2015-01-15T12:34:56+00:00"
-- @
--
-- ==== <http://tools.ietf.org/html/rfc822#section-5 RFC822>
-- @
-- > 'formatTime' 'defaultTimeLocale' "%a, %_d %b %Y %H:%M:%S %Z" $ 'mkUTCTime' 2015 1 15  12 34 56.78
-- "Thu, 15 Jan 2015 12:34:56 UTC"
-- @
--
-- ==== YYYY-MM-DD hh:mm:ss.000000
-- @
-- > 'formatTime' 'defaultTimeLocale' "%Y-%m-%d %H:%M:%S.%v" $ 'mkUTCTime' 2015 1 15  12 34 56.78
-- "2015-01-15 12:34:56.780000"
-- @
{-# INLINEABLE formatTime #-}
formatTime :: (FormatTime t)
    => TimeLocale
        -- ^ Locale for formatting.
    -> String
        -- ^ Template spec string.
    -> t
        -- ^ 'FormatTime' instance value to be formatted.
    -> String
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

-- | 'ShowS' a year with a minimum of four digits (even in the first
-- millennium), to comply with
-- <https://en.wikipedia.org/wiki/ISO_8601#Years ISO 8601 Years>.
{-# INLINE showsY #-}
showsY :: Year -> ShowS
#if BUG_FOR_BUG
showsY = shows
#else
-- ISO 8601 says minimum of 4 digits, even for first millennium.
showsY = showsYear
#endif

instance FormatTime TimeOfDay where
    {-# INLINEABLE showsTime #-}
    showsTime TimeLocale{..} (TimeOfDay h m (DiffTime s)) = \ def c -> case c of
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
        'v' -> fills06 su . shows su
        'Q' -> if su == 0 then id else (:) '.' . fills06 su . drops0 su
        -- default
        _ -> def c
        where (fromIntegral -> si, Micro su) = microQuotRem s (Micro 1000000)

instance FormatTime YearMonthDay where
    {-# INLINEABLE showsTime #-}
    showsTime TimeLocale {..} (YearMonthDay y m d) = \ def c -> case c of
        -- aggregate
        'D' -> shows02 m . (:) '/' . shows02 d . (:) '/' . shows02 (mod y 100)
        'F' -> showsY y . (:) '-' . shows02 m . (:) '-' . shows02 d
        -- Year
        'Y' -> showsY y
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
        'Y' -> showsY y
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
        'G' -> showsY y
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
        'Y' -> showsY y
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
        'Y' -> showsY y
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
    showsTime l d@(view ordinalDate -> ordinal)
        = showsTime l ordinal
        . showsTime l (ordinal ^. yearMonthDay)
        . showsTime l (toWeekOrdinal ordinal d)
        . showsTime l (toSundayOrdinal ordinal d)
        . showsTime l (toMondayOrdinal ordinal d)

instance FormatTime TimeZone where
    {-# INLINEABLE showsTime #-}
    showsTime _ tz@(TimeZone _ _ name) = \ def c -> case c of
        'z' -> (++) (timeZoneOffsetString tz)
        'N' -> (++) (timeZoneOffsetStringColon tz)
        'Z' -> (++) (if null name then timeZoneOffsetString tz else name)
        _ -> def c

instance FormatTime ZonedTime where
    {-# INLINEABLE showsTime #-}
    showsTime l (ZonedTime lt tz) = showsTime l lt . showsTime l tz

instance FormatTime UTCTime where
    {-# INLINEABLE showsTime #-}
    showsTime l t = \ def c -> case c of
        's' -> shows . fst $ qr s (Micro 1000000)
        _ -> showsTime l ((utc, t) ^. zonedTime) def c
      where
        NominalDiffTime s = t ^. posixTime
#if BUG_FOR_BUG
        qr = microDivMod -- rounds down
#else
        qr = microQuotRem -- rounds to 0
#endif

instance FormatTime UniversalTime where
    {-# INLINEABLE showsTime #-}
    showsTime l t = showsTime l $ ZonedTime lt utc {timeZoneName = "UT1"} where
        lt = t ^. ut1LocalTime 0

instance FormatTime AbsoluteTime where
    {-# INLINEABLE showsTime #-}
    showsTime l t = showsTime l $ ZonedTime lt utc {timeZoneName = "TAI"} where
        lt = t ^. from (absoluteTime tum) . utcLocalTime utc
        tum = TAIUTCMap mempty mempty

------------------------------------------------------------------------

data TimeFlag
    = PostMeridiem
    | TwelveHour
    | HasCentury
    | IsPOSIXTime
    | IsOrdinalDate
    | IsGregorian
    | IsWeekDate
    | IsSundayWeek
    | IsMondayWeek
    deriving (Enum, Show)

-- | Unconstituted date-time for parsing.
data TimeParse = TimeParse
    { tpCentury :: {-# UNPACK #-}!Int
    , tpCenturyYear :: {-# UNPACK #-}!Int{-YearOfCentury-}
    , tpMonth :: {-# UNPACK #-}!Month
    , tpWeekOfYear :: {-# UNPACK #-}!WeekOfYear
    , tpDayOfMonth :: {-# UNPACK #-}!DayOfMonth
    , tpDayOfYear :: {-# UNPACK #-}!DayOfYear
    , tpDayOfWeek :: {-# UNPACK #-}!DayOfWeek
    , tpFlags :: {-# UNPACK #-}!Int{-BitSet TimeFlag-}
    , tpHour :: {-# UNPACK #-}!Hour
    , tpMinute :: {-# UNPACK #-}!Minute
    , tpSecond :: {-# UNPACK #-}!Int
    , tpSecFrac :: {-# UNPACK #-}!DiffTime
    , tpPOSIXTime :: {-# UNPACK #-}!POSIXTime
    , tpTimeZone :: !TimeZone
    } deriving (Show)

LENS(TimeParse,tpCentury,Int)
LENS(TimeParse,tpCenturyYear,Int{-YearOfCentury-})
LENS(TimeParse,tpMonth,Month)
LENS(TimeParse,tpWeekOfYear,WeekOfYear)
LENS(TimeParse,tpDayOfMonth,DayOfMonth)
LENS(TimeParse,tpDayOfWeek,DayOfWeek)
LENS(TimeParse,tpDayOfYear,DayOfYear)
LENS(TimeParse,tpFlags,Int{-BitSet TimeFlag-})
LENS(TimeParse,tpHour,Hour)
LENS(TimeParse,tpMinute,Minute)
LENS(TimeParse,tpSecond,Int)
LENS(TimeParse,tpSecFrac,DiffTime)
LENS(TimeParse,tpPOSIXTime,POSIXTime)
LENS(TimeParse,tpTimeZone,TimeZone)

{-# INLINE flag #-}
flag :: TimeFlag -> Lens' TimeParse Bool
flag (fromEnum -> f) = _tpFlags . lens
    (`testBit` f) (\ n b -> (if b then setBit else clearBit) n f)

-- | Produce a 'Parser' for UTF-8 encoded 'ByteString's.
--
-- This function is used internally by 'parseTime', 'readTime', and 'readsTime';
-- consider using one of those functions instead.
--
-- Attoparsec easily beats any 'String' parser out there, but we do have to
-- be careful to convert the input to UTF-8 'ByteString's.
{-# INLINEABLE timeParser #-}
timeParser
    :: TimeLocale
        -- ^ Locale.
    -> String
        -- ^ Parser template spec string. See 'formatTime' for spec.
    -> Parser TimeParse
timeParser TimeLocale {..} = flip execStateT unixEpoch . go where

    go :: String -> StateT TimeParse Parser ()
    go spec = case spec of
        '%' : cspec : rspec -> case cspec of
            -- aggregate
            'c' -> go (dateTimeFmt ++ rspec)
            'r' -> go (time12Fmt ++ rspec)
            'X' -> go (timeFmt ++ rspec)
            'x' -> go (dateFmt ++ rspec)
            'R' -> go ("%H:%M" ++ rspec)
            'T' -> go ("%H:%M:%S" ++ rspec)
            'D' -> go ("%m/%d/%y" ++ rspec)
            'F' -> go ("%Y-%m-%d" ++ rspec)
            -- AM/PM
            'P' -> dayHalf
            'p' -> dayHalf
            -- Hour
            'H' -> lift (dec0 2) >>= setHour24
            'I' -> lift (dec0 2) >>= setHour12
            'k' -> (lift (dec_ 2) >>= setHour24)
                <|> (lift (dec_ 1) >>= setHour24)
            'l' -> (lift (dec_ 2) >>= setHour12)
                <|> (lift (dec_ 1) >>= setHour12)
            -- Minute
            'M' -> lift (dec0 2) >>= assign _tpMinute >> go rspec
            -- Second
            'S' -> lift (dec0 2) >>= assign _tpSecond >> go rspec
            'q' -> lift micro >>= assign _tpSecFrac . DiffTime >> go rspec
            'v' -> lift micro >>= assign _tpSecFrac . DiffTime >> go rspec
            'Q' -> lift ((P.char '.' >> DiffTime <$> micro) <|> return zeroV)
                >>= assign _tpSecFrac >> go rspec

            -- Year
            'Y' -> fullYear
            'y' -> lift (dec0 2) >>= setCenturyYear
            'C' -> lift (dec0 2) >>= setCentury
            -- Month
            'B' -> lift (indexOfCI $ fst <$> months) >>= setMonth . succ
            'b' -> lift (indexOfCI $ snd <$> months) >>= setMonth . succ
            'h' -> lift (indexOfCI $ snd <$> months) >>= setMonth . succ
            'm' -> lift (dec0 2) >>= setMonth
            -- DayOfMonth
            'd' -> lift (dec0 2) >>= setDayOfMonth
            'e' -> (lift (dec_ 2) >>= setDayOfMonth)
                <|> (lift (dec_ 1) >>= setDayOfMonth)
            -- DayOfYear
            'j' -> lift (dec0 3) >>= assign _tpDayOfYear
                >> flag IsOrdinalDate .= True >> go rspec

            -- Year (WeekDate)
            -- FIXME: problematic if input contains both %Y and %G
            'G' -> flag IsWeekDate .= True >> fullYear
            'g' -> flag IsWeekDate .= True >> lift (dec0 2) >>= setCenturyYear
            'f' -> flag IsWeekDate .= True >> lift (dec0 2) >>= setCentury
            -- WeekOfYear
            -- FIXME: problematic if more than one of the following
            'V' -> flag IsWeekDate .= True >> lift (dec0 2) >>= setWeekOfYear
            'U' -> flag IsSundayWeek .= True >> lift (dec0 2) >>= setWeekOfYear
            'W' -> flag IsMondayWeek .= True >> lift (dec0 2) >>= setWeekOfYear
            -- DayOfWeek
            'w' -> lift (dec0 1) >>= setDayOfWeek
            'u' -> lift (dec0 1) >>= setDayOfWeek
            'A' -> lift (indexOfCI $ fst <$> wDays) >>= setDayOfWeek
            'a' -> lift (indexOfCI $ snd <$> wDays) >>= setDayOfWeek

            -- TimeZone
            'z' -> do tzOffset; go rspec
            'N' -> do tzOffset; go rspec
            'Z' -> do tzOffset <|> tzName; go rspec
            -- UTCTime
            's' -> do
                s <- lift (negative P.decimal)
                _tpPOSIXTime .= fromSeconds (s :: Int64)
                flag IsPOSIXTime .= True
                go rspec

            -- modifier (whatever)
            '-' -> go ('%' : rspec)
            '_' -> go ('%' : rspec)
            '0' -> go ('%' : rspec)
            -- escape (why would anyone need %t and %n?)
            '%' -> lift (P.char '%') >> go rspec
            _ -> lift . fail $ "Unknown format character: " ++ show cspec

          where
            dayHalf = do
                pm <- lift $ False <$ stringCI (fst amPm)
                    <|> True <$ stringCI (snd amPm)
                flag PostMeridiem .= pm
                flag TwelveHour .= True
                go rspec
            -- NOTE: if a greedy parse fails or causes a later failure,
            -- then backtrack and only accept 4-digit years; see #5.
            fullYear = year (negative P.decimal) <|> year (dec0 4) where
                year p = do
                    (c, y) <- (`divMod` 100) <$> lift p
                    flag HasCentury .= True
                    _tpCentury .= c
                    _tpCenturyYear .= y
                    go rspec
            setHour12 h = do
                flag TwelveHour .= True
                _tpHour .= h
                go rspec
            setHour24 h = do
                flag TwelveHour .= False
                _tpHour .= h
                go rspec
            setCenturyYear y = do _tpCenturyYear .= y; go rspec
            setCentury c = do
                _tpCentury .= c
                flag HasCentury .= True
                go rspec
            setMonth m = do
                flag IsGregorian .= True
                _tpMonth .= m
                go rspec
            setDayOfMonth d = do
                flag IsGregorian .= True
                _tpDayOfMonth .= d
                go rspec
            setWeekOfYear w = do _tpWeekOfYear .= w; go rspec
            setDayOfWeek d = do _tpDayOfWeek .= d; go rspec
            tzOffset = do
                s <- lift (id <$ P.char '+' <|> negate <$ P.char '-')
                h <- lift (dec0 2)
                () <$ lift (P.char ':') <|> pure ()
                m <- lift (dec0 2)
                _tpTimeZone . _timeZoneMinutes .= s (h * 60 + m)
            tzName = lift timeZoneParser >>= assign _tpTimeZone

        c : rspec | P.isSpace c ->
            lift (P.takeWhile P.isSpace) >> go (dropWhile P.isSpace rspec)
        c : rspec | isAscii c -> lift (P.char c) >> go rspec
        c : rspec -> lift (charU8 c) >> go rspec
        "" -> return ()

    {-# INLINE micro #-}
    micro :: Parser Micro
    micro = do
        us10 <- either fail return . P.parseOnly P.decimal . S.take 7
            . (`S.append` S.pack "000000") =<< P.takeWhile1 P.isDigit
        return $ Micro (div (us10 + 5) 10)

    {-# INLINE unixEpoch #-}
    unixEpoch :: TimeParse
    unixEpoch = TimeParse {..} where
        tpCentury = 19
        tpCenturyYear = 70
        tpMonth = 1
        tpWeekOfYear = 1
        tpDayOfYear = 1
        tpDayOfMonth = 1
        tpDayOfWeek = 4
        tpFlags = 0
        tpHour = 0
        tpMinute = 0
        tpSecond = 0
        tpSecFrac = zeroV
        tpPOSIXTime = zeroV
        tpTimeZone = utc

-- | Parse a string as a 'ParseTime' instance value.
--
-- Return 'Nothing' if parsing fails.
--
-- === Examples
--
-- ==== <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
-- @
-- > 'parseTime' 'defaultTimeLocale' "%Y-%m-%dT%H:%M:%S%N" "2015-01-15T12:34:56+00:00" :: 'Maybe' 'UTCTime'
--   Just 2015-01-15 12:34:56 UTC
--
-- > 'parseTime' 'defaultTimeLocale' "%Y-%m-%dT%H:%M:%S%N" "2015-01-15T12:34:56-12:00" :: 'Maybe' 'UTCTime'
--   Just 2015-01-16 00:34:56 UTC
-- @
--
-- ==== YYYY-MM-DD hh:mm:ss.0
-- @
-- > 'parseTime' 'defaultTimeLocale' "%Y-%m-%d %H:%M:%S%Q" "2015-01-15 12:34:56.78" :: 'Maybe' 'UTCTime'
--   Just 2015-01-15 12:34:56.78 UTC
-- @
{-# INLINEABLE parseTime #-}
parseTime :: (ParseTime t)
    => TimeLocale
        -- ^ Locale.
    -> String
        -- ^ Parser template spec string. See 'formatTime' for spec.
    -> String
        -- ^ String value to be parsed as a 'ParseTime' instance value.
    -> Maybe t
parseTime l spec = either (const Nothing) Just
        . P.parseOnly parser . utf8String where
    parser = buildTime <$ P.skipSpace <*> timeParser l spec
        <* P.skipSpace <* P.endOfInput

-- | Parse a string as a 'ParseTime' instance value.
--
-- Call 'error' if parsing fails.
{-# INLINEABLE readTime #-}
readTime :: (ParseTime t)
    => TimeLocale
        -- ^ Locale.
    -> String
        -- ^ Parser template spec string. See 'formatTime' for spec.
    -> String
        -- ^ String value to be parsed as a 'ParseTime' instance value.
    -> t
readTime l spec = either error id . P.parseOnly parser . utf8String where
    parser = buildTime <$ P.skipSpace <*> timeParser l spec
        <* P.skipSpace <* P.endOfInput

-- | Produce a 'ReadS' to parse a string as a 'ParseTime' instance value.
{-# INLINEABLE readsTime #-}
readsTime :: (ParseTime t)
    => TimeLocale
        -- ^ Locale.
    -> String
        -- ^ Parser template spec string. See 'formatTime' for spec.
    -> ReadS t
readsTime l spec = parserToReadS $
    buildTime <$ P.skipSpace <*> timeParser l spec

------------------------------------------------------------------------

deriving instance Read UTCView
#if SHOW_INTERNAL
deriving instance Read Day
deriving instance Read TimeOfDay
deriving instance Read LocalTime
deriving instance Read ZonedTime
deriving instance Read TimeZone
instance Read UTCTime where
    {-# INLINE readsPrec #-}
    readsPrec n = fmap (first $ review utcTime) . readsPrec n
#else
instance Read Day where
    {-# INLINEABLE readsPrec #-}
    readsPrec _ = readParen False $
        readsTime defaultTimeLocale "%Y-%m-%d"

instance Read TimeOfDay where
    {-# INLINEABLE readsPrec #-}
    readsPrec _ = readParen False $
        readsTime defaultTimeLocale "%H:%M:%S%Q"

instance Read LocalTime where
    {-# INLINEABLE readsPrec #-}
    readsPrec _ = readParen False $
        readsTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

instance Read ZonedTime where
    {-# INLINEABLE readsPrec #-}
    readsPrec _ = readParen False $
        readsTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z"

instance Read UTCTime where
    {-# INLINEABLE readsPrec #-}
    readsPrec _ = readParen False $
        readsTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z"
#endif

------------------------------------------------------------------------

-- | All instances of this class may be parsed by 'parseTime', 'readTime', and
-- 'readsTime'.
class ParseTime t where
    buildTime :: TimeParse -> t

instance ParseTime TimeOfDay where
    {-# INLINE buildTime #-}
    buildTime tp@TimeParse {..} = TimeOfDay h tpMinute
            (fromSeconds tpSecond ^+^ tpSecFrac) where
        h = case tp ^. flag TwelveHour of
            False -> tpHour
            True -> case tp ^. flag PostMeridiem of
                False -> mod tpHour 12
                True -> if tpHour < 12 then tpHour + 12 else tpHour

{-# INLINE tpYear #-}
tpYear :: TimeParse -> Year
tpYear tp@TimeParse {..} = tpCenturyYear + 100 * if tp ^. flag HasCentury
    then tpCentury else if tpCenturyYear < 69 then 20 else 19

instance ParseTime YearMonthDay where
    {-# INLINE buildTime #-}
    buildTime tp@TimeParse {..} = YearMonthDay (tpYear tp) tpMonth tpDayOfMonth

instance ParseTime MonthDay where
    {-# INLINE buildTime #-}
    buildTime TimeParse {..} = MonthDay tpMonth tpDayOfMonth

instance ParseTime OrdinalDate where
    {-# INLINE buildTime #-}
    buildTime tp@TimeParse {..} = OrdinalDate (tpYear tp) tpDayOfYear

instance ParseTime WeekDate where
    {-# INLINE buildTime #-}
    buildTime tp@TimeParse {..} = WeekDate (tpYear tp) tpWeekOfYear
        (if tpDayOfWeek == 0 then 7 else tpDayOfWeek)

instance ParseTime SundayWeek where
    {-# INLINE buildTime #-}
    buildTime tp@TimeParse {..} = SundayWeek (tpYear tp) tpWeekOfYear
        (if tpDayOfWeek == 7 then 0 else tpDayOfWeek)

instance ParseTime MondayWeek where
    {-# INLINE buildTime #-}
    buildTime tp@TimeParse {..} = MondayWeek (tpYear tp) tpWeekOfYear
        (if tpDayOfWeek == 0 then 7 else tpDayOfWeek)

instance ParseTime LocalTime where
    {-# INLINE buildTime #-}
    buildTime = LocalTime <$> buildTime <*> buildTime

instance ParseTime Day where
    {-# INLINE buildTime #-}
    buildTime tp
        | tp ^. flag IsOrdinalDate = ordinalDate # buildTime tp
        | tp ^. flag IsGregorian = gregorian # buildTime tp
        | tp ^. flag IsWeekDate = weekDate # buildTime tp
        | tp ^. flag IsSundayWeek = sundayWeek # buildTime tp
        | tp ^. flag IsMondayWeek = mondayWeek # buildTime tp
        | otherwise = ordinalDate # buildTime tp
        -- TODO: Better conflict handling when multiple flags are set?

instance ParseTime TimeZone where
    {-# INLINE buildTime #-}
    buildTime = tpTimeZone

instance ParseTime ZonedTime where
    {-# INLINE buildTime #-}
    buildTime = ZonedTime <$> buildTime <*> buildTime

instance ParseTime UTCTime where
    {-# INLINE buildTime #-}
    buildTime tp@TimeParse {..} = if tp ^. flag IsPOSIXTime
        then posixTime # tpPOSIXTime
        else snd $ buildTime tp ^. from zonedTime

instance ParseTime UniversalTime where
    {-# INLINE buildTime #-}
    buildTime (buildTime -> UTCRep t) = UniversalRep t

instance ParseTime AbsoluteTime where
    {-# INLINE buildTime #-}
    buildTime tp = buildTime tp ^. absoluteTime (TAIUTCMap mempty mempty)

------------------------------------------------------------------------

-- Dubiously pilfered from time-1.4.0.2
-- s/^.*-- \(.*\)\n.*\("[A-Z]\+"\).*"\([+-]\)\([0-9]\{2\}\):\([0-9]\{2\}\)", \(True\|False\).*$/    <|> zone \2 (($\3) \4 \5) \6 -- \1/
-- followed by !sort -r , because some names are prefixes of others.
timeZoneParser :: Parser TimeZone
timeZoneParser = zone "TAI" 0 False <|> zone "UT1" 0 False

    <|> zone "ZULU" (($+) 00 00) False --  Same as UTC
    <|> zone "Z" (($+) 00 00) False --  Same as UTC
    <|> zone "YST" (($-) 09 00) False -- Yukon Standard Time
    <|> zone "YDT" (($-) 08 00) True -- Yukon Daylight-Saving Time
    <|> zone "WST" (($+) 08 00) False -- West Australian Standard Time
    <|> zone "WETDST" (($+) 01 00) True -- Western European Daylight-Saving Time
    <|> zone "WET" (($+) 00 00) False --  Western European Time
    <|> zone "WDT" (($+) 09 00) True -- West Australian Daylight-Saving Time
    <|> zone "WAT" (($-) 01 00) False -- West Africa Time
    <|> zone "WAST" (($+) 07 00) False -- West Australian Standard Time
    <|> zone "WADT" (($+) 08 00) True -- West Australian Daylight-Saving Time
    <|> zone "UTC" (($+) 00 00) False --  Universal Coordinated Time
    <|> zone "UT" (($+) 00 00) False --  Universal Time
    <|> zone "TFT" (($+) 05 00) False -- Kerguelen Time
    <|> zone "SWT" (($+) 01 00) False -- Swedish Winter Time
    <|> zone "SST" (($+) 02 00) False -- Swedish Summer Time
    <|> zone "SET" (($+) 01 00) False -- Seychelles Time
    <|> zone "SCT" (($+) 04 00) False -- Mahe Island Time
    <|> zone "SAST" (($+) 09 30) False -- South Australia Standard Time
    <|> zone "SADT" (($+) 10 30) True -- South Australian Daylight-Saving Time
    <|> zone "RET" (($+) 04 00) False -- Reunion Island Time
    <|> zone "PST" (($-) 08 00) False -- Pacific Standard Time
    <|> zone "PDT" (($-) 07 00) True -- Pacific Daylight-Saving Time
    <|> zone "NZT" (($+) 12 00) False -- New Zealand Time
    <|> zone "NZST" (($+) 12 00) False -- New Zealand Standard Time
    <|> zone "NZDT" (($+) 13 00) True -- New Zealand Daylight-Saving Time
    <|> zone "NT" (($-) 11 00) False -- Nome Time
    <|> zone "NST" (($-) 03 30) False -- Newfoundland Standard Time
    <|> zone "NOR" (($+) 01 00) False -- Norway Standard Time
    <|> zone "NFT" (($-) 03 30) False -- Newfoundland Standard Time
    <|> zone "NDT" (($-) 02 30) True -- Newfoundland Daylight-Saving Time
    <|> zone "MVT" (($+) 05 00) False -- Maldives Island Time
    <|> zone "MUT" (($+) 04 00) False -- Mauritius Island Time
    <|> zone "MT" (($+) 08 30) False -- Moluccas Time
    <|> zone "MST" (($-) 07 00) False -- Mountain Standard Time
    <|> zone "MMT" (($+) 06 30) False -- Myanmar Time
    <|> zone "MHT" (($+) 09 00) False -- Kwajalein Time
    <|> zone "MEZ" (($+) 01 00) False -- Mitteleuropaeische Zeit
    <|> zone "MEWT" (($+) 01 00) False -- Middle European Winter Time
    <|> zone "METDST" (($+) 02 00) True -- Middle Europe Daylight-Saving Time
    <|> zone "MET" (($+) 01 00) False -- Middle European Time
    <|> zone "MEST" (($+) 02 00) False -- Middle European Summer Time
    <|> zone "MDT" (($-) 06 00) True -- Mountain Daylight-Saving Time
    <|> zone "MAWT" (($+) 06 00) False -- Mawson (Antarctica) Time
    <|> zone "MART" (($-) 09 30) False -- Marquesas Time
    <|> zone "LIGT" (($+) 10 00) False -- Melbourne, Australia
    <|> zone "KST" (($+) 09 00) False -- Korea Standard Time
    <|> zone "JT" (($+) 07 30) False -- Java Time
    <|> zone "JST" (($+) 09 00) False -- Japan Standard Time, Russia zone 8
    <|> zone "IT" (($+) 03 30) False -- Iran Time
    <|> zone "IST" (($+) 02 00) False -- Israel Standard Time
    <|> zone "IRT" (($+) 03 30) False -- Iran Time
    <|> zone "IOT" (($+) 05 00) False -- Indian Chagos Time
    <|> zone "IDLW" (($-) 12 00) False -- International Date Line, West
    <|> zone "IDLE" (($+) 12 00) False -- International Date Line, East
    <|> zone "HST" (($-) 10 00) False -- Hawaii Standard Time
    <|> zone "HMT" (($+) 03 00) False -- Hellas Mediterranean Time (?)
    <|> zone "HDT" (($-) 09 00) True -- Hawaii/Alaska Daylight-Saving Time
    <|> zone "GST" (($+) 10 00) False -- Guam Standard Time, Russia zone 9
    <|> zone "GMT" (($+) 00 00) False --  Greenwich Mean Time
    <|> zone "FWT" (($+) 02 00) False -- French Winter Time
    <|> zone "FST" (($+) 01 00) False -- French Summer Time
    <|> zone "FNT" (($-) 02 00) False -- Fernando de Noronha Time
    <|> zone "FNST" (($-) 01 00) False -- Fernando de Noronha Summer Time
    <|> zone "EST" (($-) 05 00) False -- Eastern Standard Time
    <|> zone "EETDST" (($+) 03 00) True -- Eastern Europe Daylight-Saving Time
    <|> zone "EET" (($+) 02 00) False -- Eastern European Time, Russia zone 1
    <|> zone "EDT" (($-) 04 00) True -- Eastern Daylight-Saving Time
    <|> zone "EAT" (($+) 03 00) False -- Antananarivo, Comoro Time
    <|> zone "EAST" (($+) 10 00) False -- East Australian Standard Time
    <|> zone "EAST" (($+) 04 00) False -- Antananarivo Summer Time
    <|> zone "DNT" (($+) 01 00) False -- Dansk Normal Tid
    <|> zone "CXT" (($+) 07 00) False -- Christmas (Island) Time
    <|> zone "CST" (($-) 06 00) False -- Central Standard Time
    <|> zone "CETDST" (($+) 02 00) True -- Central European Daylight-Saving Time
    <|> zone "CET" (($+) 01 00) False -- Central European Time
    <|> zone "CEST" (($+) 02 00) False -- Central European Summer Time
    <|> zone "CDT" (($-) 05 00) True -- Central Daylight-Saving Time
    <|> zone "CCT" (($+) 08 00) False -- China Coastal Time
    <|> zone "CAT" (($-) 10 00) False -- Central Alaska Time
    <|> zone "CAST" (($+) 09 30) False -- Central Australia Standard Time
    <|> zone "CADT" (($+) 10 30) True -- Central Australia Daylight-Saving Time
    <|> zone "BT" (($+) 03 00) False -- Baghdad Time
    <|> zone "BST" (($+) 01 00) False -- British Summer Time
    <|> zone "BRT" (($-) 03 00) False -- Brasilia Time
    <|> zone "BRST" (($-) 02 00) False -- Brasilia Summer Time
    <|> zone "BDST" (($+) 02 00) False -- British Double Summer Time
    <|> zone "AWT" (($-) 03 00) False -- (unknown)
    <|> zone "AWST" (($+) 08 00) False -- Australia Western Standard Time
    <|> zone "AWSST" (($+) 09 00) False -- Australia Western Summer Standard Time
    <|> zone "AST" (($-) 04 00) False -- Atlantic Standard Time (Canada)
    <|> zone "ALMT" (($+) 06 00) False -- Almaty Time
    <|> zone "ALMST" (($+) 07 00) False -- Almaty Summer Time
    <|> zone "AKST" (($-) 09 00) False -- Alaska Standard Time
    <|> zone "AKDT" (($-) 08 00) True -- Alaska Daylight-Saving Time
    <|> zone "AHST" (($-) 10 00) False -- Alaska/Hawaii Standard Time
    <|> zone "AFT" (($+) 04 30) False -- Afghanistan Time
    <|> zone "AEST" (($+) 10 00) False -- Australia Eastern Standard Time
    <|> zone "AESST" (($+) 11 00) False -- Australia Eastern Summer Standard Time
    <|> zone "ADT" (($-) 03 00) True -- Atlantic Daylight-Saving Time
    <|> zone "ACT" (($-) 05 00) False -- Atlantic/Porto Acre Standard Time
    <|> zone "ACST" (($-) 04 00) False -- Atlantic/Porto Acre Summer Time
    <|> zone "ACSST" (($+) 10 30) False -- Central Australia Summer Standard Time

  where
    zone name offset dst = TimeZone offset dst name <$ P.string (S.pack name)
    ($+) h m = h * 60 + m
    ($-) h m = negate (h * 60 + m)

