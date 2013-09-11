{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Micro
import Data.Thyme.Calendar.Internal
import Data.VectorSpace
import System.Random
import Test.QuickCheck

#if !SHOW_INTERNAL
import Control.Monad
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (char)
import Text.Read (readPrec)
#endif

-- | Time differences, encompassing both 'DiffTime' and 'NominalDiffTime'.
--
-- FIXME: still affected by <http://hackage.haskell.org/trac/ghc/ticket/7611>?
class (HasBasis t, Basis t ~ (), Scalar t ~ Rational) => TimeDiff t where
    -- | Escape hatch; avoid.
    microseconds :: Iso' t Int64

-- | Convert a time difference to some 'Fractional' type.
{-# INLINE toSeconds #-}
toSeconds :: (TimeDiff t, Fractional n) => t -> n
toSeconds = (* recip 1000000) . fromIntegral . view microseconds

-- | Make a time difference from some 'Real' type.
--
-- Where speed is a concern, make sure @n@ is one of 'Float', 'Double',
-- 'Int', 'Int64' or 'Integer', for which @RULES@ have been provided.
{-# INLINE fromSeconds #-}
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

"fromSeconds/Float"     fromSeconds = fromSecondsRealFrac (0 :: Float)
"fromSeconds/Double"    fromSeconds = fromSecondsRealFrac (0 :: Double)
"fromSeconds/Int"       fromSeconds = fromSecondsIntegral (0 :: Int)
"fromSeconds/Int64"     fromSeconds = fromSecondsIntegral (0 :: Int64)
"fromSeconds/Integer"   fromSeconds = fromSecondsIntegral (0 :: Integer)

  #-}

------------------------------------------------------------------------

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

{-# INLINE posixDayLength #-}
posixDayLength :: NominalDiffTime
posixDayLength = microseconds # 86400000000

------------------------------------------------------------------------

-- Represented as a 'NominalDiffTime' since MJD epoch.
newtype UniversalTime = UniversalRep NominalDiffTime deriving (INSTANCES_MICRO)

{-# INLINE modJulianDate #-}
modJulianDate :: Iso' UniversalTime Rational
modJulianDate = iso
    (\ (UniversalRep t) -> toSeconds t / toSeconds posixDayLength)
    (UniversalRep . (*^ posixDayLength))

------------------------------------------------------------------------

-- Represented as a 'NominalDiffTime' since MJD epoch.
newtype UTCTime = UTCRep NominalDiffTime deriving (INSTANCES_MICRO)

data UTCView = UTCTime
    { utctDay :: {-# UNPACK #-}!Day
    , utctDayTime :: {-# UNPACK #-}!DiffTime
    } deriving (INSTANCES_USUAL, Show)

instance NFData UTCView

_utctDay :: Lens' UTCTime Day
_utctDay = utcTime . lens utctDay (\ (UTCTime _ t) d -> UTCTime d t)

_utctDayTime :: Lens' UTCTime DiffTime
_utctDayTime = utcTime . lens utctDayTime (\ (UTCTime d _) t -> UTCTime d t)

instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    {-# INLINE (.-.) #-}
    UTCRep a .-. UTCRep b = a ^-^ b
    {-# INLINE (.+^) #-}
    UTCRep a .+^ d = UTCRep (a ^+^ d)

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

