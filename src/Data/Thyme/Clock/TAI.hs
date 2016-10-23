{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}


#if HLINT
#include "cabal_macros.h"
#endif

#define TAIUTCDAT @<http://maia.usno.navy.mil/ser7/tai-utc.dat tai-utc.dat>@

-- | <https://en.wikipedia.org/wiki/International_Atomic_Time International Atomic Time>
-- (TAI) and conversion to/from UTC, accounting for leap seconds.
module Data.Thyme.Clock.TAI
    ( AbsoluteTime
    , taiEpoch
    , TAIUTCMap (..)
    , TAIUTCRow (..)
    , absoluteTime
    , absoluteTime'
    , utcDayLength
    , parseTAIUTCRow
    , makeTAIUTCMap
    , parseTAIUTCDAT

    -- * Compatibility
    , addAbsoluteTime
    , diffAbsoluteTime
    , utcToTAITime
    , taiToUTCTime
    ) where

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mempty)
#endif
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Attoparsec.ByteString.Char8 ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as S
import Data.Data
import Data.Hashable
import Data.Ix
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Thyme.Calendar
import Data.Thyme.Clock.Internal
import Data.Thyme.Format.Internal (indexOf)
import Data.Thyme.Internal.Micro
import Data.Thyme.LocalTime
#if __GLASGOW_HASKELL__ == 704
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
#endif
import Data.Vector.Unboxed.Deriving
import Data.VectorSpace
import GHC.Generics (Generic)
import System.Random (Random)
import Test.QuickCheck

-- | <https://en.wikipedia.org/wiki/International_Atomic_Time Temps Atomique International>
-- (TAI). Note that for most applications 'UTCTime' is perfectly sufficient,
-- and much more convenient to use.
--
-- Internally this is the number of seconds since 'taiEpoch'. TAI days are
-- exactly 86400 SI seconds long.
newtype AbsoluteTime = AbsoluteTime DiffTime deriving (Eq, Ord, Data, Typeable, Generic, Enum, Ix, Hashable, NFData, Bounded, Random, Arbitrary, CoArbitrary)

derivingUnbox "AbsoluteTime" [t| AbsoluteTime -> DiffTime |]
    [| \ (AbsoluteTime a) -> a |] [| AbsoluteTime |]

instance Show AbsoluteTime where
    {-# INLINEABLE showsPrec #-}
    showsPrec p tai = showsPrec p lt . (++) " TAI" where
        lt = tai ^. from (absoluteTime $ TAIUTCMap mempty mempty) . utcLocalTime utc

-- | The <http://en.wikipedia.org/wiki/Julian_day#Variants Modified Julian Day>
-- epoch, which is /1858-11-17 00:00:00 TAI/.
{-# INLINE taiEpoch #-}
taiEpoch :: AbsoluteTime
taiEpoch = AbsoluteTime zeroV

instance AffineSpace AbsoluteTime where
    type Diff AbsoluteTime = DiffTime
    {-# INLINE (.-.) #-}
    (.-.) = \ (AbsoluteTime a) (AbsoluteTime b) -> a ^-^ b
    {-# INLINE (.+^) #-}
    (.+^) = \ (AbsoluteTime a) d -> AbsoluteTime (a ^+^ d)

-- | A table of 'TAIUTCRow's for converting between TAI and UTC.
--
-- The two 'Map's are keyed on the corresponding instants in UTC and TAI
-- from which the 'TAIUTCRow' becomes applicable. The 'UTCTime' key of the
-- first 'Map' is always at midnight.
--
-- No table is provided here because leap seconds are unpredictable, and any
-- program shipped with such a table could become out-of-date in as little
-- as 6 months. See 'parseTAIUTCDAT' for details.
data TAIUTCMap = TAIUTCMap (Map UTCTime TAIUTCRow) (Map AbsoluteTime TAIUTCRow)
    deriving (Eq, Ord, Data, Typeable, Generic, Show)

-- | Each line of TAIUTCDAT (see 'parseTAIUTCDAT') specifies the difference
-- between TAI and UTC for a particular period. For example:
--
-- @
-- 1968 FEB  1 =JD 2439887.5  TAI-UTC=   4.2131700 S + (MJD - 39126.) X 0.002592 S
-- @
--
-- says that from 1968-02-01 00:00:00 (Julian Date 2439887.5; or Modified
-- Julian Date 39887.0), the difference between TAI and UTC is @4.2131700s@
-- (the /additive/ part) plus a scaled component that increases for each day
-- beyond MJD 39126 (the /base/) by 0.002592s (the /coefficient/). In
-- general, the latter half of each line is of the form:
--
-- @
-- TAI-UTC= /additive/ S + (MJD - /base/) X /coefficient/ S
-- @
--
-- @'TAIUTCRow' a b c@ is a normalised version of the above, with the /base/
-- multiplied by 86400s, and the /coefficient/ divided by the same. This
-- allows us to use the internal representation of 'UTCTime'—seconds since
-- the MJD epoch—as the @MJD@ term without further rescaling.
--
-- Note that between 1961-01-01 and 1972-01-01, each UTC second was actually
-- slightly longer than one TAI (or SI) second. For the first year this was
-- at the rate of exactly 1.000000015 TAI (or SI) seconds per UTC second,
-- but was subject to irregular updates. Since leap seconds came into effect
-- on 1972-01-01, the /additive/ part has always been an intergral number of
-- seconds, and the /coefficient/ has always been zero.
--
-- To convert between TAI and UTC, we refer to the definition:
--
-- @
-- TAI - UTC = a + (MJD - b) * c
-- @
--
-- Using UTC for MJD (with 'b' and 'c' scaled as described above):
--
-- @
-- TAI = UTC + a + (UTC - b) * c
-- TAI - a + b * c = UTC + UTC * c
-- (TAI - a + b * c) / (1 + c) = UTC
-- @
--
-- This is implemented by 'absoluteTime' and 'absoluteTime''.
--
-- Further reading:
--
-- * https://en.wikipedia.org/wiki/Coordinated_Universal_Time
-- * https://en.wikipedia.org/wiki/International_Atomic_Time
data TAIUTCRow = TAIUTCRow !DiffTime !UTCTime !Rational
    -- ^ Each row comprises of an /additive/ component, the /base/ of the
    -- scaled component, and the /coefficient/ of the scaled component.
    deriving (Eq, Ord, Data, Typeable, Generic, Show)

{-# INLINE lookupLE #-}
lookupLE :: (Ord k) => k -> Map k TAIUTCRow -> TAIUTCRow
lookupLE k = maybe (TAIUTCRow zeroV (UTCRep zeroV) 0) snd . Map.lookupLE k

{-# INLINE unwrap #-}
unwrap :: TAIUTCRow -> (Micro, Micro, Rational)
unwrap (TAIUTCRow (DiffTime a) (UTCRep (NominalDiffTime b)) c) = (a, b, c)

-- | Convert between 'UTCTime' and 'AbsoluteTime' using a 'TAIUTCMap'.
--
-- Since 'UTCTime' cannot represent a time-of-day of 86400s or more, any
-- conversion from 'AbsoluteTime' that happens to be during a leap second
-- will overflow into the next day.
--
-- See 'parseTAIUTCDAT' for how to obtain the @tum :: 'TAIUTCMap'@ below.
--
-- @
-- > let jul1 = 'utcTime' 'Control.Lens.#' 'UTCView' ('gregorian' 'Control.Lens.#' 'YearMonthDay' 2015 7 1) 'zeroV'
-- > jul1 '&' 'absoluteTime' tum '%~' ('.-^' 'fromSeconds' 1.1)
-- 2015-06-30 23:59:59.9 UTC
-- @
{-# INLINE absoluteTime #-}
absoluteTime :: TAIUTCMap -> Iso' UTCTime AbsoluteTime
absoluteTime (TAIUTCMap utcMap taiMap) = iso toTAI toUTC where
    {-# INLINEABLE toTAI #-}
    toTAI :: UTCTime -> AbsoluteTime
    toTAI utime@(UTCRep (NominalDiffTime uts)) = AbsoluteTime . DiffTime $
            uts ^+^ a ^+^ (uts ^-^ b) ^* c where
        (a, b, c) = unwrap $ lookupLE utime utcMap

    {-# INLINEABLE toUTC #-}
    toUTC :: AbsoluteTime -> UTCTime
    toUTC atime@(AbsoluteTime (DiffTime ats)) = UTCRep . NominalDiffTime $
            (ats ^-^ a ^+^ b ^* c) ^/ (1 + c) where
        (a, b, c) = unwrap $ lookupLE atime taiMap

-- | Convert between 'UTCView' and TAI 'AbsoluteTime' using a 'TAIUTCMap'.
--
-- Unlike 'absoluteTime', 'UTCView' /can/ represent a time-of-day greater
-- than 86400s, and this gives the correct results during a leap second.
--
-- See 'parseTAIUTCDAT' for how to obtain the @tum :: 'TAIUTCMap'@ below.
--
-- @
-- > let jul1 = 'UTCView' ('gregorian' 'Control.Lens.#' 'YearMonthDay' 2015 7 1) 'zeroV'
-- > jul1 '&' 'absoluteTime'' tum '%~' ('.-^' 'fromSeconds' 0.1)
-- 'UTCView' {'utcvDay' = 2015-06-30, 'utcvDayTime' = 86400.9s}
-- @
--
-- However keep in mind that currently there is no standard way to get the
-- TAI on most platforms. Simply converting the result of
-- 'Data.Thyme.Clock.getCurrentTime' (which calls @gettimeofday(2)@) to
-- 'AbsoluteTime' during a leap second will still give non-monotonic times.
{-# INLINE absoluteTime' #-}
absoluteTime' :: TAIUTCMap -> Iso' UTCView AbsoluteTime
absoluteTime' (TAIUTCMap utcMap taiMap) = iso toTAI toUTC where
    {-# INLINEABLE toTAI #-}
    toTAI :: UTCView -> AbsoluteTime
    toTAI uview@(UTCView day _) = AbsoluteTime . DiffTime $
            uts ^+^ a ^+^ (uts ^-^ b) ^* c where
        (a, b, c) = unwrap $ lookupLE (utcTime # UTCView day zeroV) utcMap
        UTCRep (NominalDiffTime uts) = utcTime # uview

    {-# INLINEABLE toUTC #-}
    toUTC :: AbsoluteTime -> UTCView
    toUTC atime@(AbsoluteTime (DiffTime ats)) = fixup (utime ^. utcTime) where
        row@(unwrap -> (a, b, c)) = lookupLE atime taiMap
        utime = UTCRep . NominalDiffTime $ (ats ^-^ a ^+^ b ^* c) ^/ (1 + c)
        -- 'lookupLE' of the same instant in 'utcMap' and 'taiMap' should
        -- give the same 'TAIUTCRow'. If it doesn't, then @utime@ must have
        -- overflown into the next 'Day'.
        fixup uview@(UTCView day dt) = if lookupLE utime utcMap == row
            then uview else UTCView (day .-^ 1) (fromSeconds' 86400 ^+^ dt)

-- TODO: Linux >= 3.10 has @CLOCK_TAI@ for @clock_gettime(2)@.

-- | Using a 'TAIUTCMap', lookup the 'DiffTime' length of the UTC 'Day'.
--
-- See 'parseTAIUTCDAT' for how to obtain the @tum :: 'TAIUTCMap'@ below.
--
-- @
-- > 'utcDayLength' tum '.' 'view' '_utctDay' '<$>' 'getCurrentTime'
-- 86400s
-- > 'utcDayLength' tum '$' 'gregorian' 'Control.Lens.#' 'YearMonthDay' 2015 6 30
-- 86401s
-- @
utcDayLength :: TAIUTCMap -> Day -> DiffTime
utcDayLength tum day = diff (day .+^ 1) .-. diff day where
    diff d = UTCView d zeroV ^. from utcTime . absoluteTime tum

-- | @attoparsec@ 'P.Parser' for a single line of TAIUTCDAT.
--
-- Returns the starting 'UTCTime' and the normalised 'TAIUTCRow'.
parseTAIUTCRow :: P.Parser (UTCTime, TAIUTCRow)
parseTAIUTCRow = do
    y <- P.skipSpace *> P.decimal <?> "Year"
    let months = [ "JAN", "FEB", "MAR", "APR", "MAY", "JUN"
            , "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" ]
    m <- (+) 1 <$ P.skipSpace <*> indexOf months <?> "Month"
    d <- P.skipSpace *> P.decimal <?> "DayOfMonth"

    tokens ["=", "JD"]
    -- TAI-UTC changes always happen at midnight UTC, so just ignore ".5".
    since <- subtract 2400000{-.5-} <$> P.decimal
        <* P.string ".5" <?> "Julian Date .5"
    let ymd = YearMonthDay y m d
    unless (gregorian # ymd == ModifiedJulianDay since) $ do
        fail $ show ymd ++ " ≠ MJD " ++ show since
            ++ " ≡ " ++ show (ModifiedJulianDay since)

    tokens ["TAI", "-", "UTC", "="]
    a <- P.rational <?> "Additive"
    tokens ["S", "+", "(", "MJD", "-"]
    b <- P.decimal <* P.char '.' <?> "Base" -- also always midnight UTC
    tokens [")", "X"]
    c <- (/ toSeconds' posixDayLength) <$> P.rational
        <* P.skipSpace <* P.string "S" <?> "Coefficient"

    return (mjdToUTC since, TAIUTCRow (fromSeconds' a) (mjdToUTC b) c)
  where
    tokens ts = foldr (\ tok a -> P.skipSpace >> P.string tok >> a)
        P.skipSpace ts <?> ("tokens " ++ show ts)
    mjdToUTC mjd = utcTime # UTCView (ModifiedJulianDay mjd) zeroV

-- | Build a 'TAIUTCMap' from the result of 'parseTAIUTCRow'.
makeTAIUTCMap :: [(UTCTime, TAIUTCRow)] -> TAIUTCMap
makeTAIUTCMap rows = TAIUTCMap (Map.fromList rows)
        (Map.fromList $ invert <$> rows) where
    invert (since, entry) = (since ^. absoluteTime single, entry) where
        single = TAIUTCMap (Map.singleton since entry) mempty

-- | Parse the contents of TAIUTCDAT into a 'TAIUTCMap' for conversion
-- between TAI and UTC.
--
-- @
-- $ curl -O \"http:\/\/maia.usno.navy.mil\/ser7\/tai-utc.dat\"
-- $ ghci --package thyme
-- > import "Data.Thyme"
-- > import "Data.Thyme.Clock.TAI"
-- > import "Data.ByteString" ('S.readFile')
-- > Right tum \<- 'parseTAIUTCDAT' '<$>' 'S.readFile' \"tai-utc.dat\"
-- > 'utcDayLength' tum '$' 'gregorian' 'Control.Lens.#' 'YearMonthDay' 2015 6 30
-- 86401s
-- @
parseTAIUTCDAT :: S.ByteString -> Either String TAIUTCMap
parseTAIUTCDAT = P.parseOnly $ makeTAIUTCMap <$> P.manyTill
    (parseTAIUTCRow <* P.endOfLine) P.endOfInput

------------------------------------------------------------------------

-- | Add a duration to an 'AbsoluteTime'.
--
-- @
-- 'addAbsoluteTime' = 'flip' ('.+^')
-- 'addAbsoluteTime' d t ≡ t '.+^' d
-- @
--
-- See also the 'AffineSpace' instance for 'AbsoluteTime'.
{-# INLINE addAbsoluteTime #-}
addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime = flip (.+^)

-- | The duration difference between two 'AbsoluteTime's.
--
-- @
-- 'diffAbsoluteTime' = ('.-.')
-- 'diffAbsoluteTime' a b ≡ a '.-.' b
-- @
--
-- See also the 'AffineSpace' instance for 'AbsoluteTime'.
{-# INLINE diffAbsoluteTime #-}
diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime = (.-.)

-- | Using a 'TAIUTCMap', convert a 'UTCTime' to 'AbsoluteTime'.
--
-- @
-- 'utcToTAITime' = 'view' '.' 'absoluteTime'
-- @
{-# INLINE utcToTAITime #-}
utcToTAITime :: TAIUTCMap -> UTCTime -> AbsoluteTime
utcToTAITime = view . absoluteTime

-- | Using a 'TAIUTCMap', convert a 'AbsoluteTime' to 'UTCTime'.
--
-- @
-- 'taiToUTCTime' = 'review' '.' 'absoluteTime'
-- @
{-# INLINE taiToUTCTime #-}
taiToUTCTime :: TAIUTCMap -> AbsoluteTime -> UTCTime
taiToUTCTime = review . absoluteTime
