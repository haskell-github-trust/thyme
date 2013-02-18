{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Num', 'Real', 'Fractional' and 'RealFrac' instances for 'DiffTime' and
-- 'NominalDiffTime' are only available by importing "Data.Thyme.Time". In
-- their stead, instances of 'Data.AdditiveGroup.AdditiveGroup',
-- 'Data.Basis.HasBasis' and 'Data.VectorSpace.VectorSpace' are given here.
-- Addition and subtraction are performed with the 'Data.AdditiveGroup.^+^'
-- and 'Data.AdditiveGroup.^-^' operators from
-- 'Data.AdditiveGroup.AdditiveGroup', while 'Data.VectorSpace.*^' scales by
-- a 'Rational'. To find the ratio of two time differences, define:
--
-- @
-- (^\/^) :: ('Data.Basis.HasBasis' v, 'Data.Basis.Basis' v ~ (), 'Data.VectorSpace.Scalar' v ~ s, 'Prelude.Fractional' s) => v -> v -> s
-- (^\/^) = ('Prelude./') '`Data.Function.on`' flip 'Data.Basis.decompose'' ()
-- @
--
-- Finally, write @n 'Data.VectorSpace.*^' 'Data.Basis.basisValue' ()@ where
-- literals are expected.
--
-- 'UniversalTime' and 'UTCTime' are instances of
-- 'Data.AffineSpace.AffineSpace', with @'Data.AffineSpace.Diff'
-- 'UniversalTime' ≡ 'DiffTime'@, and @'Data.AffineSpace.Diff' 'UTCTime' ≡
-- 'NominalDiffTime'@.

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

