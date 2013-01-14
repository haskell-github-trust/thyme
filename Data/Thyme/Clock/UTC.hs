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
import Data.Bits
import Data.Data
import Data.Int
import Data.Ix
import Data.Micro
import Data.Thyme.Calendar.Day
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
    basisValue () = 1
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

newtype UTCTime = UTCPacked Int64
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
    (view utcTime -> UTCTime da ta) .-. (view utcTime -> UTCTime db tb) =
        fromIntegral (da .-. db) *^ posixDayLength ^+^ NominalDiffTime td where
        DiffTime td = ta ^-^ tb
    {-# INLINE (.+^) #-}
    (view utcTime -> UTCTime day (DiffTime dt)) .+^ NominalDiffTime d
        = review utcTime $ UTCTime day (DiffTime (dt ^+^ d))

{-# INLINE utcTime #-}
utcTime :: Simple Iso UTCTime UTCView
utcTime = iso unpack pack where

    {-# INLINE unpack #-}
    unpack :: UTCTime -> UTCView
    unpack (UTCPacked n) = UTCTime
            (ModifiedJulianDay mjd) (DiffTime (Micro dt)) where
        mjd = shiftR n bitsDayTime
        dt = n .&. maskDayTime

    {-# INLINE pack #-}
    pack :: UTCView -> UTCTime
    pack (UTCTime (ModifiedJulianDay mjd) (DiffTime dt)) =
            UTCPacked (shiftL (mjd + dd) bitsDayTime .|. pt) where
        NominalDiffTime posixDay = posixDayLength
        (dd, Micro pt) = microQuotRem dt posixDay

    {-# INLINE bitsDayTime #-}
    bitsDayTime :: Int
    bitsDayTime = 37 -- enough for 86400 microseconds

    {-# INLINE maskDayTime #-}
    maskDayTime :: Int64
    maskDayTime = shiftL 1 bitsDayTime - 1

