{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- #hide
module Data.Thyme.Clock.Scale where

import Prelude
import Control.DeepSeq
import Data.AdditiveGroup
import Data.Basis
import Data.Data
import Data.Int
import Data.Ix
import Data.Micro
import Data.VectorSpace

-- TODO: UniversalTime

newtype DiffTime = DiffTime Micro
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable, AdditiveGroup)

#if SHOW_INTERNAL
deriving instance Show DiffTime
#else
instance Show DiffTime where
    showsPrec p (DiffTime a) rest = showsPrec p a ('s' : rest)
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

#if INSTANCE_NUM
deriving instance Num DiffTime
deriving instance Real DiffTime
deriving instance Fractional DiffTime
deriving instance RealFrac DiffTime
#endif

{-# INLINE microsecondsToDiffTime #-}
microsecondsToDiffTime :: Int64 -> DiffTime
microsecondsToDiffTime = DiffTime . Micro
