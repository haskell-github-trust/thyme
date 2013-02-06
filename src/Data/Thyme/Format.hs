{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Thyme.Format
    ( FormatTime (..)
    , formatTime
    , ParseTime (..)
    , parseTime
    , readTime
    , readsTime
    , TimeParse (..)
    , timeParser
    ) where

import Prelude
import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Basis
import Data.Bits
import qualified Data.ByteString.Char8 as S
import Data.Char
import Data.Micro
import Data.Thyme.Calendar
import Data.Thyme.Calendar.Internal
import Data.Thyme.Calendar.MonthDay
import Data.Thyme.Clock.Internal
import Data.Thyme.Clock.POSIX
import Data.Thyme.Clock.TAI
import Data.Thyme.Format.Internal
import Data.Thyme.LocalTime
import Data.Thyme.TH
import Data.VectorSpace
import System.Locale

class FormatTime t where
    showsTime :: TimeLocale -> t -> (Char -> ShowS) -> Char -> ShowS

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
    showsTime l d@(view ordinalDate -> ordinal)
        = showsTime l ordinal
        . showsTime l (view yearMonthDay ordinal)
        . showsTime l (toWeekOrdinal ordinal d)
        . showsTime l (toSundayOrdinal ordinal d)
        . showsTime l (toMondayOrdinal ordinal d)

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

instance FormatTime UniversalTime where
    {-# INLINEABLE showsTime #-}
    showsTime l t = showsTime l $ ZonedTime lt utc {timeZoneName = "UT1"} where
        lt = view (ut1LocalTime 0) t

instance FormatTime AbsoluteTime where
    {-# INLINEABLE showsTime #-}
    showsTime l t = showsTime l $ ZonedTime lt utc {timeZoneName = "TAI"} where
        lt = view (from (absoluteTime $ const zeroV) . utcLocalTime utc) t

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

thymeLenses ''TimeParse

{-# INLINE flag #-}
flag :: TimeFlag -> Lens' TimeParse Bool
flag (fromEnum -> f) = _tpFlags . lens
    (`testBit` f) (\ n b -> (if b then setBit else clearBit) n f)

-- | Time 'Parser' for UTF-8 encoded 'ByteString's.
--
-- Attoparsec easily beats any 'String' parser out there, but we do have to
-- be careful to convert the input to UTF-8 'ByteString's.
{-# INLINEABLE timeParser #-}
timeParser :: TimeLocale -> String -> Parser TimeParse
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
            'k' -> lift (dec_ 2 <|> dec_ 1) >>= setHour24
            'l' -> lift (dec_ 2 <|> dec_ 1) >>= setHour12
            -- Minute
            'M' -> lift (dec0 2) >>= assign _tpMinute >> go rspec
            -- Second
            'S' -> lift (dec0 2) >>= assign _tpSecond >> go rspec
            'q' -> lift micro >>= assign _tpSecFrac . DiffTime >> go rspec
            'Q' -> lift ((P.char '.' >> DiffTime <$> micro) <|> return zeroV)
                >>= assign _tpSecFrac >> go rspec

            -- Year
            -- FIXME: should full years / centuries be fixed width?
            'Y' -> lift (dec0 4) >>= setYear
            'y' -> lift (dec0 2) >>= setCenturyYear
            'C' -> lift (dec0 2) >>= setCentury
            -- Month
            'B' -> lift (indexOfCI $ fst <$> months) >>= setMonth . succ
            'b' -> lift (indexOfCI $ snd <$> months) >>= setMonth . succ
            'h' -> lift (indexOfCI $ snd <$> months) >>= setMonth . succ
            'm' -> lift (dec0 2) >>= setMonth
            -- DayOfMonth
            'd' -> lift (dec0 2) >>= setDayOfMonth
            'e' -> lift (dec_ 2 <|> dec_ 1) >>= setDayOfMonth
            -- DayOfYear
            'j' -> lift (dec0 3) >>= assign _tpDayOfYear
                >> flag IsOrdinalDate .= True >> go rspec

            -- Year (WeekDate)
            -- FIXME: problematic if input contains both %Y and %G
            'G' -> flag IsWeekDate .= True >> lift (dec0 4) >>= setYear
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
            'Z' -> do tzOffset <|> tzName; go rspec
            -- UTCTime
            's' -> do
                s <- lift (negative P.decimal)
                _tpPOSIXTime .= fromIntegral s *^ basisValue ()
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
            setHour12 h = do
                flag TwelveHour .= True
                _tpHour .= h
                go rspec
            setHour24 h = do
                flag TwelveHour .= False
                _tpHour .= h
                go rspec
            setYear ((`divMod` 100) -> (c, y)) = do
                flag HasCentury .= True
                _tpCentury .= c
                _tpCenturyYear .= y
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
        "" -> lift P.skipSpace

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

{-# INLINEABLE parseTime #-}
parseTime :: (ParseTime t) => TimeLocale -> String -> String -> Maybe t
parseTime l spec = either (const Nothing) Just
    . P.parseOnly (buildTime <$> timeParser l spec) . utf8String

{-# INLINEABLE readTime #-}
readTime :: (ParseTime t) => TimeLocale -> String -> String -> t
readTime l spec = either error id
    . P.parseOnly (buildTime <$> timeParser l spec) . utf8String

{-# INLINEABLE readsTime #-}
readsTime :: (ParseTime t) => TimeLocale -> String -> ReadS t
readsTime l spec = parserToReadS (buildTime <$> timeParser l spec)

------------------------------------------------------------------------

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

------------------------------------------------------------------------

class ParseTime t where
    buildTime :: TimeParse -> t

instance ParseTime TimeOfDay where
    {-# INLINE buildTime #-}
    buildTime tp@TimeParse {..} = TimeOfDay h tpMinute
            (fromIntegral tpSecond *^ basisValue () ^+^ tpSecFrac) where
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
    buildTime tp@TimeParse {..}
        | tp ^. flag IsOrdinalDate = review ordinalDate (buildTime tp)
        | tp ^. flag IsGregorian = review gregorian (buildTime tp)
        | tp ^. flag IsWeekDate = review weekDate (buildTime tp)
        | tp ^. flag IsSundayWeek = review sundayWeek (buildTime tp)
        | tp ^. flag IsMondayWeek = review mondayWeek (buildTime tp)
        | otherwise = review ordinalDate (buildTime tp)
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
        then review posixTime tpPOSIXTime
        else view (from zonedTime . _2) (buildTime tp)

instance ParseTime UniversalTime where
    {-# INLINE buildTime #-}
    buildTime (buildTime -> UTCRep t) = UniversalRep t

instance ParseTime AbsoluteTime where
    {-# INLINE buildTime #-}
    buildTime = view (absoluteTime $ const zeroV) <$> buildTime

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

