{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-} -- workaround
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

#include "thyme.h"

module Data.Thyme.Clock.Internal where

import Prelude
import Control.DeepSeq
import Control.Lens
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Basis
import Data.Data
import Data.Hashable
import Data.Int
import Data.Ix
import Data.Thyme.Internal.Micro
import Data.Thyme.Calendar.Internal
#if __GLASGOW_HASKELL__ == 704
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
#endif
import Data.Vector.Unboxed.Deriving
import Data.VectorSpace
import GHC.Generics (Generic)
import System.Random
import Test.QuickCheck

#if !SHOW_INTERNAL
import Control.Monad
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (char)
import Text.Read (readPrec)
#endif

-- | Hour time-of-day.
type Hour = Int
-- | Minute time-of-day.
type Minute = Int

-- | Time intervals, encompassing both 'DiffTime' and 'NominalDiffTime'.
--
-- ==== Notes
--
-- Still affected by <http://hackage.haskell.org/trac/ghc/ticket/7611>?
class (HasBasis t, Basis t ~ (), Scalar t ~ Rational) => TimeDiff t where
    -- | Conversion between 'TimeDiff' and 'Int64' microseconds.
    --
    -- @
    -- > ('fromSeconds'' 3 :: 'DiffTime') '^.' 'microseconds'
    -- 3000000
    --
    -- > 'microseconds' 'Control.Lens.#' 4000000 :: 'DiffTime'
    -- 4s
    -- @
    microseconds :: Iso' t Int64

-- | Convert a time interval to some 'Fractional' type.
{-# INLINE toSeconds #-}
toSeconds :: (TimeDiff t, Fractional n) => t -> n
toSeconds = (* recip 1000000) . fromIntegral . view microseconds

-- | Make a time interval from some 'Real' type.
--
-- Try to make sure @n@ is one of 'Float', 'Double', 'Int', 'Int64' or
-- 'Integer', for which rewrite @RULES@ have been provided.
{-# INLINE[0] fromSeconds #-}
fromSeconds :: (Real n, TimeDiff t) => n -> t
fromSeconds = fromSeconds' . toRational

-- | Type-restricted 'toSeconds' to avoid constraint-defaulting warnings.
{-# INLINE toSeconds' #-}
toSeconds' :: (TimeDiff t) => t -> Rational
toSeconds' = (`decompose'` ())

-- | Type-restricted 'fromSeconds' to avoid constraint-defaulting warnings.
{-# INLINE fromSeconds' #-}
fromSeconds' :: (TimeDiff t) => Rational -> t
fromSeconds' = (*^ basisValue ())

------------------------------------------------------------------------
-- not for public consumption

{-# INLINE fromSecondsRealFrac #-}
fromSecondsRealFrac :: (RealFrac n, TimeDiff t) => n -> n -> t
fromSecondsRealFrac _ = review microseconds . round . (*) 1000000

{-# INLINE fromSecondsIntegral #-}
fromSecondsIntegral :: (Integral n, TimeDiff t) => n -> n -> t
fromSecondsIntegral _ = review microseconds . (*) 1000000 . fromIntegral

{-# RULES

"fromSeconds/Float"    [~0] fromSeconds = fromSecondsRealFrac (0 :: Float)
"fromSeconds/Double"   [~0] fromSeconds = fromSecondsRealFrac (0 :: Double)
"fromSeconds/Int"      [~0] fromSeconds = fromSecondsIntegral (0 :: Int)
"fromSeconds/Int64"    [~0] fromSeconds = fromSecondsIntegral (0 :: Int64)
"fromSeconds/Integer"  [~0] fromSeconds = fromSecondsIntegral (0 :: Integer)

  #-}

------------------------------------------------------------------------

-- | An interval or duration of time, as would be measured by a stopwatch.
--
-- 'DiffTime' is an instance of 'AdditiveGroup' as well as 'VectorSpace',
-- with 'Rational' as its 'Scalar'.
-- We do not provide 'Num', 'Real', 'Fractional' nor 'RealFrac' instances
-- here. See "Data.Thyme.Docs#spaces" for details.
--
-- @
-- > 'fromSeconds'' 100 :: 'DiffTime'
-- 100s
-- > 'fromSeconds'' 100 '^+^' 'fromSeconds'' 100 '^*' 4
-- 500s
-- > 'fromSeconds'' 100 '^-^' 'fromSeconds'' 100 '^/' 4
-- 75s
-- @
newtype DiffTime = DiffTime Micro deriving (INSTANCES_MICRO, AdditiveGroup)

derivingUnbox "DiffTime" [t| DiffTime -> Micro |]
    [| \ (DiffTime a) -> a |] [| DiffTime |]

#if SHOW_INTERNAL
deriving instance Show DiffTime
deriving instance Read DiffTime
#else
instance Show DiffTime where
    {-# INLINEABLE showsPrec #-}
    showsPrec p (DiffTime a) = showsPrec p a . (:) 's'
instance Read DiffTime where
    {-# INLINEABLE readPrec #-}
    readPrec = return (const . DiffTime) `ap` readPrec `ap` lift (char 's')
#endif

instance VectorSpace DiffTime where
    type Scalar DiffTime = Rational
    {-# INLINE (*^) #-}
    (*^) = \ s (DiffTime t) -> DiffTime (s *^ t)

instance HasBasis DiffTime where
    type Basis DiffTime = ()
    {-# INLINE basisValue #-}
    basisValue = \ _ -> DiffTime (basisValue ())
    {-# INLINE decompose #-}
    decompose = \ (DiffTime a) -> decompose a
    {-# INLINE decompose' #-}
    decompose' = \ (DiffTime a) -> decompose' a

instance TimeDiff DiffTime where
    {-# INLINE microseconds #-}
    microseconds = iso (\ (DiffTime (Micro u)) -> u) (DiffTime . Micro)

------------------------------------------------------------------------

-- | The nominal interval between two 'UTCTime's, which does not take leap
-- seconds into account.
--
-- For example, the difference between /23:59:59/ and /00:00:01/ on the
-- following day is always 2 seconds of 'NominalDiffTime', regardless of
-- whether a leap-second took place.
--
-- 'NominalDiffTime' is an instance of 'AdditiveGroup' as well as
-- 'VectorSpace', with 'Rational' as its 'Scalar'.
-- We do not provide 'Num', 'Real', 'Fractional' nor 'RealFrac' instances
-- here. See "Data.Thyme.Docs#spaces" for details.
--
-- @
-- > let d = 'fromSeconds'' 2 :: 'NominalDiffTime'
-- > d
-- 2s
-- > d '^/' 3
-- 0.666667s
-- @
--
-- See also: 'UTCTime'.
newtype NominalDiffTime = NominalDiffTime Micro deriving (INSTANCES_MICRO, AdditiveGroup)

derivingUnbox "NominalDiffTime" [t| NominalDiffTime -> Micro |]
    [| \ (NominalDiffTime a) -> a |] [| NominalDiffTime |]

#if SHOW_INTERNAL
deriving instance Show NominalDiffTime
deriving instance Read NominalDiffTime
#else
instance Show NominalDiffTime where
    {-# INLINEABLE showsPrec #-}
    showsPrec p (NominalDiffTime a) rest = showsPrec p a ('s' : rest)
instance Read NominalDiffTime where
    {-# INLINEABLE readPrec #-}
    readPrec = return (const . NominalDiffTime) `ap` readPrec `ap` lift (char 's')
#endif

instance VectorSpace NominalDiffTime where
    type Scalar NominalDiffTime = Rational
    {-# INLINE (*^) #-}
    (*^) = \ s (NominalDiffTime t) -> NominalDiffTime (s *^ t)

instance HasBasis NominalDiffTime where
    type Basis NominalDiffTime = ()
    {-# INLINE basisValue #-}
    basisValue = \ _ -> NominalDiffTime (basisValue ())
    {-# INLINE decompose #-}
    decompose = \ (NominalDiffTime a) -> decompose a
    {-# INLINE decompose' #-}
    decompose' = \ (NominalDiffTime a) -> decompose' a

instance TimeDiff NominalDiffTime where
    {-# INLINE microseconds #-}
    microseconds = iso (\ (NominalDiffTime (Micro u)) -> u) (NominalDiffTime . Micro)

-- | The nominal length of a POSIX day: /86400 SI seconds/.
{-# INLINE posixDayLength #-}
posixDayLength :: NominalDiffTime
posixDayLength = microseconds # 86400000000

------------------------------------------------------------------------

-- | The principal form of universal time, namely
-- <http://en.wikipedia.org/wiki/Universal_Time#Versions UT1>.
--
-- UT1 is defined by the rotation of the Earth around its axis relative to
-- the Sun. The length of each UT1 day varies and is never exactly 86400 SI
-- seconds, unlike UTC or TAI.
--
-- The difference between UT1 and UTC is
-- <http://en.wikipedia.org/wiki/DUT1 DUT1>.
newtype UniversalTime = UniversalRep NominalDiffTime deriving (INSTANCES_MICRO)

derivingUnbox "UniversalTime" [t| UniversalTime -> NominalDiffTime |]
    [| \ (UniversalRep a) -> a |] [| UniversalRep |]

-- | Convert between 'UniversalTime' and the fractional number of days since the
-- <http://en.wikipedia.org/wiki/Julian_day#Variants Modified Julian Date epoch>.
{-# INLINE modJulianDate #-}
modJulianDate :: Iso' UniversalTime Rational
modJulianDate = iso
    (\ (UniversalRep t) -> toSeconds t / toSeconds posixDayLength)
    (UniversalRep . (*^ posixDayLength))

#if __GLASGOW_HASKELL__ >= 710
pattern UniversalTime :: Rational -> UniversalTime
pattern UniversalTime mjd <- (view modJulianDate -> mjd) where
    UniversalTime mjd = modJulianDate # mjd
#elif __GLASGOW_HASKELL__ >= 708
pattern UniversalTime mjd <- (view modJulianDate -> mjd)
#endif

------------------------------------------------------------------------

-- | <https://en.wikipedia.org/wiki/Coordinated_Universal_Time Coördinated universal time>
-- ('UTCTime') is the most commonly used standard for civil timekeeping. It
-- is synchronised with
-- <https://en.wikipedia.org/wiki/International_Atomic_Time TAI>
-- ('Data.Thyme.Clock.AbsoluteTime') and both tick in increments of SI
-- seconds, but UTC includes occasional leap-seconds to keep it close to
-- <https://en.wikipedia.org/wiki/Universal_Time#Versions UT1>
-- ('UniversalTime').
--
-- @
-- > 'utcTime' 'Control.Lens.#' 'UTCView' ('gregorian' 'Control.Lens.#' 'YearMonthDay' 2016 1 15) ('Data.Thyme.LocalTime.timeOfDay' 'Control.Lens.#' 'Data.Thyme.LocalTime.TimeOfDay' 12 34 56.78)
-- 2016-01-15 12:34:56.78 UTC
--
-- > 'UTCTime' ('gregorian' 'Control.Lens.#' 'YearMonthDay' 2016 1 15) ('Data.Thyme.LocalTime.timeOfDay' 'Control.Lens.#' 'Data.Thyme.LocalTime.TimeOfDay' 12 34 56.78)
-- 2016-01-15 12:34:56.78 UTC
--
-- > 'mkUTCTime' 2016 1 15  12 34 56.78
-- 2016-01-15 12:34:56.78 UTC
-- @
--
-- 'UTCTime' is an 'AffineSpace' with 'NominalDiffTime' as its 'Diff'. See
-- "Data.Thyme.Docs#spaces" for details.
--
-- @
-- > let t0 = 'mkUTCTime' 2016 1 15  23 59 0
-- > let t1 = 'mkUTCTime' 2016 1 16  00  1 1
-- > let dt = t1 '.-.' t0
-- > dt
-- 121s :: 'NominalDiffTime'
--
-- > t1 '.+^' dt
-- 2016-01-16 00:03:02 UTC
--
-- > t1 '.+^' 3 '*^' dt
-- 2016-01-16 00:07:04 UTC
-- @
--
-- To decompose a 'UTCTime' into a separate 'Day' and time-of-day, use
-- 'utcTime'. To convert to a local time zone, see
-- 'Data.Thyme.LocalTime.zonedTime' or 'Data.Thyme.LocalTime.utcLocalTime'.
--
-- ==== Notes
--
-- Internally 'UTCTime' is just a 64-bit count of 'microseconds' since the
-- Modified Julian Day epoch, so @('.+^')@, @('.-.')@ et cetera ought to be
-- fast.
--
-- 'UTCTime' <https://github.com/liyang/thyme/issues/3 cannot represent leap seconds>.
-- If leap seconds matter, use 'Data.Thyme.Clock.TAI.AbsoluteTime' from
-- "Data.Thyme.Clock.TAI" instead, along with
-- 'Data.Thyme.Clock.TAI.absoluteTime'' and 'UTCView' for presentation.
newtype UTCTime = UTCRep NominalDiffTime deriving (INSTANCES_MICRO)

derivingUnbox "UTCTime" [t| UTCTime -> NominalDiffTime |]
    [| \ (UTCRep a) -> a |] [| UTCRep |]

-- | Unpacked 'UTCTime', partly for compatibility with @time@.
--
-- As of GHC 7.10, you can also use the 'UTCTime' pattern synonym.
data UTCView = UTCView
    { utcvDay :: {-# UNPACK #-}!Day
    -- ^ Calendar date.
    , utcvDayTime :: {-# UNPACK #-}!DiffTime
    -- ^ Time elapsed since midnight; /0/ ≤ 'utcvDayTime' < /86401s/.
    } deriving (INSTANCES_USUAL, Show)

-- | 'Lens'' for the calendar 'Day' component of a 'UTCView'.
LENS(UTCView,utcvDay,Day)

-- | 'Lens'' for the time-of-day 'DiffTime' component of a 'UTCView'.
LENS(UTCView,utcvDayTime,DiffTime)

derivingUnbox "UTCView" [t| UTCView -> (Day, DiffTime) |]
    [| \ UTCView {..} -> (utcvDay, utcvDayTime) |]
    [| \ (utcvDay, utcvDayTime) -> UTCView {..} |]

instance Hashable UTCView
instance NFData UTCView

-- | 'Lens'' for the calendar 'Day' component of a 'UTCTime'.
_utctDay :: Lens' UTCTime Day
_utctDay = utcTime . lens utcvDay
    (\ UTCView {..} d -> UTCView d utcvDayTime)

-- | 'Lens'' for the time-of-day 'DiffTime' component of a 'UTCTime'.
_utctDayTime :: Lens' UTCTime DiffTime
_utctDayTime = utcTime . lens utcvDayTime
    (\ UTCView {..} t -> UTCView utcvDay t)

-- | Accessor for the calendar 'Day' component of an 'UTCTime'.
--
-- @
-- 'utctDay' = 'view' '_utctDay'
-- @
utctDay :: UTCTime -> Day
utctDay = view _utctDay

-- | Accessor for the time-of-day 'DiffTime' component of an 'UTCTime'.
--
-- @
-- 'utctDayTime' = 'view' '_utctDayTime'
-- @
utctDayTime :: UTCTime -> DiffTime
utctDayTime = view _utctDayTime

instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    {-# INLINE (.-.) #-}
    (.-.) = \ (UTCRep a) (UTCRep b) -> a ^-^ b
    {-# INLINE (.+^) #-}
    (.+^) = \ (UTCRep a) d -> UTCRep (a ^+^ d)

-- | View 'UTCTime' as an 'UTCView', comprising a 'Day' along with
-- a 'DiffTime' offset since midnight.
--
-- This is an improper lens: 'utcvDayTime' outside the range of
-- @['zeroV', 'posixDayLength')@ will carry over into 'utcvDay', with the
-- expected behaviour.
--
-- @
-- > 'view' 'utcTime' '<$>' 'Data.Thyme.Clock.getCurrentTime'
-- 'UTCView' {'utcvDay' = 2016-01-15, 'utcvDayTime' = 49322.287688s}
--
-- > 'utcTime' 'Control.Lens.#' 'UTCView' ('gregorian' 'Control.Lens.#' 'YearMonthDay' 2016 1 15) ('Data.Thyme.LocalTime.timeOfDay' 'Control.Lens.#' 'Data.Thyme.LocalTime.TimeOfDay' 12 34 56.78)
-- 2016-01-15 12:34:56.78 UTC
-- @
--
-- With @{-# LANGUAGE ViewPatterns #-}@, you can write: e.g.
--
-- @
-- f :: 'UTCTime' -> ('Day', 'DiffTime')
-- f ('view' 'utcTime' -> 'UTCView' day dt) = (day, dt)
-- @
{-# INLINE utcTime #-}
utcTime :: Iso' UTCTime UTCView
utcTime = iso toView fromView where
    NominalDiffTime posixDay@(Micro uPosixDay) = posixDayLength

    {-# INLINE toView #-}
    toView :: UTCTime -> UTCView
    toView (UTCRep (NominalDiffTime a)) = UTCView
            (ModifiedJulianDay mjd) (DiffTime dt) where
        (fromIntegral -> mjd, dt) = microDivMod a posixDay

    {-# INLINE fromView #-}
    fromView :: UTCView -> UTCTime
    fromView (UTCView (ModifiedJulianDay mjd) (DiffTime dt)) = UTCRep a where
        a = NominalDiffTime (Micro (fromIntegral mjd * uPosixDay) ^+^ dt)

#if __GLASGOW_HASKELL__ >= 710
pattern UTCTime :: Day -> DiffTime -> UTCTime
pattern UTCTime d t <- (view utcTime -> UTCView d t) where
    UTCTime d t = utcTime # UTCView d t
#elif __GLASGOW_HASKELL__ >= 708
pattern UTCTime d t <- (view utcTime -> UTCView d t)
#endif

-- | Construct a 'UTCTime' from a 'gregorian' date and time-of-day.
--
-- @
-- 'mkUTCTime' yy mm dd h m s ≡ 'utcTime' 'Control.Lens.#' 'UTCView'
--     ('gregorian' 'Control.Lens.#' 'YearMonthDay' yy mm dd)
--     ('Data.Thyme.LocalTime.timeOfDay' 'Control.Lens.#' 'Data.Thyme.LocalTime.TimeOfDay' h m ('fromSeconds' s))
-- @
{-# INLINE mkUTCTime #-}
mkUTCTime :: Year -> Month -> DayOfMonth -> Hour -> Minute -> Double -> UTCTime
mkUTCTime yy mm dd h m s = utcTime # UTCView
    (gregorian # YearMonthDay yy mm dd)
    (fromSeconds (3600 * h + 60 * m) ^+^ fromSeconds s)

