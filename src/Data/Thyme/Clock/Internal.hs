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
-- ==== Issues
--
-- Still affected by
-- <http://hackage.haskell.org/trac/ghc/ticket/7611>?
class (HasBasis t, Basis t ~ (), Scalar t ~ Rational) => TimeDiff t where
    -- | "Control.Lens.Iso" between 'TimeDiff' and 'Int64' microseconds.
    --
    -- @
    -- 'view'   'microseconds' ≡ 'Data.Thyme.Time.Core.toMicroseconds'
    -- 'review' 'microseconds' ≡ 'Data.Thyme.Time.Core.fromMicroseconds'
    -- @
    --
    -- ==== Examples
    --
    -- @
    -- > ('fromSeconds'' 3 :: 'DiffTime') '^.' 'microseconds'
    --   3000000
    -- @
    --
    -- @
    -- > 'microseconds' 'Control.Lens.Review.#' 4000000 :: 'DiffTime'
    --   4s
    -- @
    microseconds :: Iso' t Int64

-- | Convert a time interval to some 'Fractional' type.
{-# INLINE toSeconds #-}
toSeconds :: (TimeDiff t, Fractional n) => t -> n
toSeconds = (* recip 1000000) . fromIntegral . view microseconds

-- | Make a time interval from some 'Real' type.
--
-- ==== Performance
--
-- Try to make sure @n@ is one of 'Float', 'Double', 'Int',
-- 'Int64' or 'Integer', for which rewrite @RULES@ have been provided.
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

{-# INLINE picoseconds #-}
picoseconds :: (TimeDiff t) => Iso' t Integer
picoseconds = microseconds . iso ((*) 1000000 . toInteger)
    (\ ps -> fromInteger $ quot (ps + signum ps * 500000) 1000000)

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
-- 'DiffTime' is an instance of 'AdditiveGroup', so it can be added with '^+^'
-- or subtracted with '^-^'.
--
-- 'DiffTime' is an instance of 'VectorSpace', so it can be scaled
-- using '*^', where
--
-- @
-- type 'Scalar' 'DiffTime' = 'Rational'
-- @
--
-- ==== Examples
--
-- @
-- > 'fromSeconds'' 100 :: DiffTime
--   100s
-- @
--
-- @
-- > 'fromSeconds'' 100 '^.' 'Data.Thyme.LocalTime.timeOfDay'
--   00:01:40
-- @
--
-- @
-- > 'Data.Thyme.LocalTime.timeOfDay' 'Control.Lens.Review.#' 'Data.Thyme.LocalTime.TimeOfDay' 0 1 40
--   100s
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

-- | The nominal interval between two points of 'UTCTime' which does not
-- take leap seconds into account.
--
-- For example, the difference between /23:59:59/ and /00:00:01/ on the
-- following day is always 2 seconds of 'NominalDiffTime', regardless of
-- whether a leap-second took place.
--
-- 'NominalDiffTime' is an instance of 'AdditiveGroup', so it can be added
-- with '^+^' or subtracted with '^-^'.
--
-- 'NominalDiffTime' is an instance 'VectorSpace', so it can be scaled
-- using '*^', where
--
-- @
-- type 'Scalar' 'NominalDiffTime' = 'Rational'
-- @
--
-- ==== Examples
--
-- @
-- > 'fromSeconds'' (1 % 3) :: NominalDiffTime
--   0.333333s
-- @
--
-- @
-- > 'hhmmss' 12 34 56.78 :: NominalDiffTime
--   45296.78s
-- @
--
-- @
-- > let t₀ = 'Data.Thyme.Time.Core.mkUTCTime' ('Data.Thyme.Time.Core.fromGregorian' 2016 1 15) ('hhmmss' 12 0 0)
-- > let t₁ = 'Data.Thyme.Time.Core.mkUTCTime' ('Data.Thyme.Time.Core.fromGregorian' 2016 1 15) ('hhmmss' 12 0 1)
-- > let δt = t₁ '.-.' t₀
--
-- > δt
--   60s
--
-- > t₀ '.+^' δt
--   2016-01-15 12:01:00 UTC
--
-- > t₀ '.+^' 3 '*^' δt
--   2016-01-15 12:03:00 UTC
-- @
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
-- 'UniversalTime' is defined by the rotation of the Earth around its axis
-- relative to the Sun. The length of each UT1 day varies and is never
-- exactly 86400 SI seconds, unlike
-- 'UTCTime' or 'AbsoluteTime'.
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

------------------------------------------------------------------------

-- | <http://en.wikipedia.org/wiki/Coordinated_Universal_Time Coördinated universal time>:
-- the most common form of universal time for civil timekeeping. It is
-- synchronised with 'AbsoluteTime' and both tick in increments of SI
-- seconds, but UTC includes occasional leap-seconds so that it does not
-- drift too far from 'UniversalTime'.
--
-- 'UTCTime' is an instance of 'AffineSpace', with
--
-- @
-- type 'Diff' 'UTCTime' = 'NominalDiffTime'
-- @
--
-- Use '.+^' to add (or '.-^' to subtract) time intervals of type
-- 'NominalDiffTime', and '.-.' to get the interval between 'UTCTime's.
--
-- To decompose a 'UTCTime' into a separate date and time, use the 'utcTime'
-- Iso.
--
-- To translate a 'UTCTime' into a local zoned time, use
-- the 'Data.Thyme.LocalTime.zonedTime' Iso.
--
-- To calculate the true absolute 'DiffTime' between two 'UTCTime's while
-- accounting for leap seconds, first convert them to
-- 'Data.Thyme.Clock.TAI.AbsoluteTime' by using
-- the 'Data.Thyme.Clock.TAI.absoluteTime' Iso and
-- then subtract with '.-.'.
--
-- ==== Performance
--
-- Internally this is a 64-bit count of 'microseconds' since
-- the Modified Julian Day epoch, so '.+^', '.-^' and '.-.' ought to be fairly
-- fast.
--
-- ==== Issues
--
-- 'UTCTime' currently
-- <https://github.com/liyang/thyme/issues/3 cannot represent leap seconds>.
-- If leap seconds were supported, then the length of a day in 'UTCTime' could
-- be /86399/, /86400/, or /86401/ seconds.
--
-- ==== Examples
--
-- @
-- > 'utcTime' 'Control.Lens.Review.#' 'UTCView' ('gregorian' 'Control.Lens.Review.#' 'YearMonthDay' 2016 1 15) ('Data.Thyme.LocalTime.timeOfDay' 'Control.Lens.Review.#' 'Data.Thyme.LocalTime.TimeOfDay' 12 34 56.78)
--   2016-01-15 12:34:56.78 UTC
--
-- > 'Data.Thyme.Time.Core.mkUTCTime' ('Data.Thyme.Time.Core.fromGregorian' 2016 1 15) ('hhmmss' 12 34 56.78)
--   2016-01-15 12:34:56.78 UTC
-- @
newtype UTCTime = UTCRep NominalDiffTime deriving (INSTANCES_MICRO)

derivingUnbox "UTCTime" [t| UTCTime -> NominalDiffTime |]
    [| \ (UTCRep a) -> a |] [| UTCRep |]

-- | Unpacked 'UTCTime', partly for compatibility with @time@.
data UTCView = UTCView
    { utcvDay :: {-# UNPACK #-}!Day
        -- ^ Calendar date.
    , utcvDayTime :: {-# UNPACK #-}!DiffTime
        -- ^ Time-of-day, time elapsed from midnight.
        --
        -- /0/ ≤ 'utctDayTime' < /86400s/
        --
        -- (If 'UTCTime' supported leap seconds, then range would be
        -- /0/ ≤ 'utctDayTime' < /86401s/.)
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
utctDay :: UTCTime -> Day
utctDay = view _utctDay

-- | Accessor for the time-of-day 'DiffTime' component of an 'UTCTime'.
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
-- This is an improper lens: 'utctDayTime' outside the range
-- of @['zeroV', 'posixDayLength')@ will carry over into the day part, with the
-- expected behaviour.
--
-- ==== Examples
--
-- @
-- > 'view' 'utcTime' \<$\> 'Data.Thyme.Clock.getCurrentTime'
--   'UTCView' {utcvDay = 2020-01-15, utcvDayTime = 49322.287688s}
--
-- > 'utcTime' 'Control.Lens.Review.#' 'UTCView' ('gregorian' 'Control.Lens.Review.#' 'YearMonthDay' 2016 1 15) ('Data.Thyme.LocalTime.timeOfDay' 'Control.Lens.Review.#' 'Data.Thyme.LocalTime.TimeOfDay' 12 34 56.78)
--   2016-01-15 12:34:56.78 UTC
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
--     ('timeOfDay' 'Control.Lens.#' 'TimeOfDay' h m ('fromSeconds' s))
-- @
{-# INLINE mkUTCTime #-}
mkUTCTime :: Year -> Month -> DayOfMonth -> Hour -> Minute -> Double -> UTCTime
mkUTCTime yy mm dd h m s = utcTime # UTCView
    (gregorian # YearMonthDay yy mm dd)
    (fromSeconds (3600 * h + 60 * m) ^+^ fromSeconds s)

