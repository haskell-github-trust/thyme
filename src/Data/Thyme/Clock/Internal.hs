{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-} -- workaround
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.Int
import Data.Ix
import Data.Thyme.Internal.Micro
import Data.Thyme.Calendar.Internal
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

-- | Time intervals, encompassing both 'DiffTime' and 'NominalDiffTime'.
--
-- [@Issues@] Still affected by
-- <http://hackage.haskell.org/trac/ghc/ticket/7611>?
class (HasBasis t, Basis t ~ (), Scalar t ~ Rational) => TimeDiff t where
    -- | Escape hatch; avoid.
    microseconds :: Iso' t Int64

-- | Convert a time interval to some 'Fractional' type.
{-# INLINE toSeconds #-}
toSeconds :: (TimeDiff t, Fractional n) => t -> n
toSeconds = (* recip 1000000) . fromIntegral . view microseconds

-- | Make a time interval from some 'Real' type.
--
-- [@Performance@] Try to make sure @n@ is one of 'Float', 'Double', 'Int',
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

------------------------------------------------------------------------
-- not for public consumption

fromSecondsRealFrac :: (RealFrac n, TimeDiff t) => n -> n -> t
fromSecondsRealFrac _ = review microseconds . round . (*) 1000000

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

-- | An absolute time interval as measured by a clock.
--
-- 'DiffTime' forms an 'AdditiveGroup'―so can be added using '^+^' (or '^-^'
-- for subtraction), and also an instance of 'VectorSpace'―so can be scaled
-- using '*^', where
--
-- @
-- type 'Scalar' 'DiffTime' = 'Rational'
-- @
newtype DiffTime = DiffTime Micro deriving (INSTANCES_MICRO, AdditiveGroup)

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
    s *^ DiffTime t = DiffTime (s *^ t)

instance HasBasis DiffTime where
    type Basis DiffTime = ()
    {-# INLINE basisValue #-}
    basisValue () = DiffTime (basisValue ())
    {-# INLINE decompose #-}
    decompose (DiffTime a) = decompose a
    {-# INLINE decompose' #-}
    decompose' (DiffTime a) = decompose' a

instance TimeDiff DiffTime where
    {-# INLINE microseconds #-}
    microseconds = iso (\ (DiffTime (Micro u)) -> u) (DiffTime . Micro)

------------------------------------------------------------------------

-- | A time interval as measured by UTC, that does not take leap-seconds
-- into account.
--
-- For instance, the difference between @23:59:59@ and @00:00:01@ on the
-- following day is always 2 seconds of 'NominalDiffTime', regardless of
-- whether a leap-second took place.
--
-- 'NominalDiffTime' forms an 'AdditiveGroup'―so can be added using '^+^'
-- (or '^-^' for subtraction), and also an instance of 'VectorSpace'―so can
-- be scaled using '*^', where
--
-- @
-- type 'Scalar' 'NominalDiffTime' = 'Rational'
-- @
newtype NominalDiffTime = NominalDiffTime Micro deriving (INSTANCES_MICRO, AdditiveGroup)

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
    s *^ NominalDiffTime t = NominalDiffTime (s *^ t)

instance HasBasis NominalDiffTime where
    type Basis NominalDiffTime = ()
    {-# INLINE basisValue #-}
    basisValue () = NominalDiffTime (basisValue ())
    {-# INLINE decompose #-}
    decompose (NominalDiffTime a) = decompose a
    {-# INLINE decompose' #-}
    decompose' (NominalDiffTime a) = decompose' a

instance TimeDiff NominalDiffTime where
    {-# INLINE microseconds #-}
    microseconds = iso (\ (NominalDiffTime (Micro u)) -> u) (NominalDiffTime . Micro)

-- | The nominal length of a POSIX day: precisely 86400 SI seconds.
{-# INLINE posixDayLength #-}
posixDayLength :: NominalDiffTime
posixDayLength = microseconds # 86400000000

------------------------------------------------------------------------

-- | The principal form of universal time, namely
-- <http://en.wikipedia.org/wiki/Universal_Time#Versions UT1>.
--
-- 'UniversalTime' is defined by the rotation of the Earth around its axis
-- relative to the Sun. Thus the length of a day by this definition varies
-- from one to the next, and is never exactly 86400 SI seconds unlike
-- <http://en.wikipedia.org/wiki/International_Atomic_Time TAI> or
-- 'AbsoluteTime'. The difference between UT1 and UTC is
-- <http://en.wikipedia.org/wiki/DUT1 DUT1>.
newtype UniversalTime = UniversalRep NominalDiffTime deriving (INSTANCES_MICRO)

-- | View 'UniversalTime' as a fractional number of days since the
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
-- [@Performance@] Internally this is a 64-bit count of 'microseconds' since
-- the MJD epoch, so '.+^', '.-^' and '.-.' ought to be fairly fast.
--
-- [@Issues@] 'UTCTime' currently
-- <https://github.com/liyang/thyme/issues/3 cannot represent leap seconds>.
newtype UTCTime = UTCRep NominalDiffTime deriving (INSTANCES_MICRO)

-- | Unpacked 'UTCTime', partly for compatibility with @time@.
data UTCView = UTCTime
    { utctDay :: {-# UNPACK #-}!Day
    , utctDayTime :: {-# UNPACK #-}!DiffTime
    } deriving (INSTANCES_USUAL, Show)

instance NFData UTCView

-- | 'Lens'' for the 'Day' component of an 'UTCTime'.
_utctDay :: Lens' UTCTime Day
_utctDay = utcTime . lens utctDay (\ (UTCTime _ t) d -> UTCTime d t)

-- | 'Lens'' for the time-of-day component of an 'UTCTime'.
_utctDayTime :: Lens' UTCTime DiffTime
_utctDayTime = utcTime . lens utctDayTime (\ (UTCTime d _) t -> UTCTime d t)

instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    {-# INLINE (.-.) #-}
    UTCRep a .-. UTCRep b = a ^-^ b
    {-# INLINE (.+^) #-}
    UTCRep a .+^ d = UTCRep (a ^+^ d)

-- | View 'UTCTime' as an 'UTCView', comprising a 'Day' along with
-- a 'DiffTime' offset since midnight.
--
-- This is an improper lens: 'utctDayTime' offsets outside the range of
-- @['zeroV', 'posixDayLength')@ will carry over into the day part, with the
-- expected behaviour.
{-# INLINE utcTime #-}
utcTime :: Iso' UTCTime UTCView
utcTime = iso toView fromView where
    NominalDiffTime posixDay@(Micro uPosixDay) = posixDayLength

    {-# INLINE toView #-}
    toView :: UTCTime -> UTCView
    toView (UTCRep (NominalDiffTime a)) = UTCTime
            (ModifiedJulianDay mjd) (DiffTime dt) where
        (fromIntegral -> mjd, dt) = microDivMod a posixDay

    {-# INLINE fromView #-}
    fromView :: UTCView -> UTCTime
    fromView (UTCTime (ModifiedJulianDay mjd) (DiffTime dt)) = UTCRep a where
        a = NominalDiffTime (Micro (fromIntegral mjd * uPosixDay) ^+^ dt)

