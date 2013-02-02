{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- #hide
module Data.Thyme.Clock.UTC where

import Prelude
import Control.DeepSeq
import Control.Lens
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Basis
import Data.Data
import Data.Ix
import Data.Micro
import Data.Thyme.Calendar
import Data.Thyme.Clock.Scale
import Data.VectorSpace

newtype NominalDiffTime = NominalDiffTime Micro
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable, AdditiveGroup)

#if SHOW_INTERNAL
deriving instance Show NominalDiffTime
#else
instance Show NominalDiffTime where
    showsPrec p (NominalDiffTime a) rest = showsPrec p a ('s' : rest)
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

#if INSTANCE_NUM
deriving instance Num NominalDiffTime
deriving instance Real NominalDiffTime
deriving instance Fractional NominalDiffTime
deriving instance RealFrac NominalDiffTime
#endif

{-# INLINE posixDayLength #-}
posixDayLength :: NominalDiffTime
posixDayLength = NominalDiffTime (toMicro 86400)

------------------------------------------------------------------------

newtype UniversalTime = UniversalRep NominalDiffTime -- since MJD epoch
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable)

{-# INLINE modJulianDate #-}
modJulianDate :: Simple Iso UniversalTime Rational
modJulianDate = iso
    (\ (UniversalRep t) -> t ^/^ posixDayLength)
    (UniversalRep . (*^ posixDayLength))

------------------------------------------------------------------------

newtype UTCTime = UTCRep NominalDiffTime -- since MJD epoch
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable)

data UTCView = UTCTime
    { utctDay :: {-# UNPACK #-}!Day
    , utctDayTime :: {-# UNPACK #-}!DiffTime
    } deriving (Eq, Ord, Data, Typeable, Show)

instance NFData UTCView

_utctDay :: Simple Lens UTCTime Day
_utctDay = utcTime . lens utctDay (\ (UTCTime _ t) d -> UTCTime d t)

_utctDayTime :: Simple Lens UTCTime DiffTime
_utctDayTime = utcTime . lens utctDayTime (\ (UTCTime d _) t -> UTCTime d t)

instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    {-# INLINE (.-.) #-}
    UTCRep a .-. UTCRep b = a ^-^ b
    {-# INLINE (.+^) #-}
    UTCRep a .+^ d = UTCRep (a ^+^ d)

{-# INLINE utcTime #-}
utcTime :: Simple Iso UTCTime UTCView
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

