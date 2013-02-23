{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Num', 'Real', 'Fractional' and 'RealFrac' instances for 'DiffTime' and
-- 'NominalDiffTime' are only available by importing "Data.Thyme.Time". In
-- their stead are instances of 'Data.AdditiveGroup.AdditiveGroup',
-- 'Data.Basis.HasBasis' and 'Data.VectorSpace.VectorSpace', with
-- @'Data.VectorSpace.Scalar' 'DiffTime' ≡ 'Data.VectorSpace.Scalar'
-- 'NominalDiffTime' ≡ 'Rational'@.
--
--  Convert between time intervals and 'Rational's with 'seconds', or more
--  generally between any 'Real' or 'Fractional' using 'fromSeconds' and
--  'toSeconds'.
--
-- 'UTCTime' is an instance of 'Data.AffineSpace.AffineSpace', with
-- @'Data.AffineSpace.Diff' 'UTCTime' ≡ 'NominalDiffTime'@.

module Data.Thyme.Clock (
    -- * Universal Time
      UniversalTime
    , modJulianDate

    -- * Absolute intervals
    , DiffTime
    , microDiffTime

    -- * UTC
    , UTCTime, UTCView (..)
    , utcTime
    , NominalDiffTime
    , microNominalDiffTime
    , module Data.Thyme.Clock

    -- * Time interval conversion
    , seconds
    , toSeconds, fromSeconds
    , toSeconds', fromSeconds'

    -- * Lenses
    , _utctDay, _utctDayTime
    ) where

import Prelude
import Control.Lens
import Data.Thyme.Clock.Internal
import Data.Thyme.Clock.POSIX
#if !SHOW_INTERNAL
import Data.Thyme.LocalTime.Internal
import Data.Thyme.LocalTime.TimeZone
#endif

#if SHOW_INTERNAL
instance Show UTCTime where
    showsPrec p = showsPrec p . view utcTime
#else
instance Show UTCTime where
    showsPrec p = showsPrec p . view zonedTime . (,) utc
#endif

getCurrentTime :: IO UTCTime
getCurrentTime = fmap (review posixTime) getPOSIXTime

