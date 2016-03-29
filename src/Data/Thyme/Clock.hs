{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif

-- | Types and functions for
-- <http://en.wikipedia.org/wiki/Coordinated_Universal_Time UTC> and
-- <http://en.wikipedia.org/wiki/Universal_Time#Versions UT1>.
--
-- If you don't care about leap seconds, keep to 'UTCTime' and
-- 'NominalDiffTime' for your clock calculations, and you'll be fine.
--
-- 'Num', 'Real', 'Fractional' and 'RealFrac' instances for 'DiffTime' and
-- 'NominalDiffTime' are only available by importing "Data.Thyme.Time". In
-- their stead are instances of 'Data.AdditiveGroup.AdditiveGroup',
-- 'Data.Basis.HasBasis' and 'Data.VectorSpace.VectorSpace', with
-- @'Data.VectorSpace.Scalar' 'DiffTime' ≡ 'Data.VectorSpace.Scalar'
-- 'NominalDiffTime' ≡ 'Rational'@.
--
--  Using 'fromSeconds' and 'toSeconds' to convert between 'TimeDiff's and
--  other numeric types. If you really must coerce between 'DiffTime' and
--  'NominalDiffTime', @'view' ('microseconds' . 'from' 'microseconds')@.
--
-- 'UTCTime' is an instance of 'Data.AffineSpace.AffineSpace', with
-- @'Data.AffineSpace.Diff' 'UTCTime' ≡ 'NominalDiffTime'@.
--
-- 'UTCTime' is not Y294K-compliant. Please file a bug report on GitHub when
-- this becomes a problem.

module Data.Thyme.Clock (
    -- * Universal Time
      UniversalTime
    , modJulianDate

    -- * Absolute intervals
    , DiffTime

    -- * UTC
    , UTCTime
#if __GLASGOW_HASKELL__ >= 708
    , pattern UTCTime
#endif
    , utctDay, utctDayTime
    , UTCView (..)
    , utcTime
    , NominalDiffTime
    , module Data.Thyme.Clock

    -- * Time interval conversion
    , TimeDiff (..)
    , toSeconds, fromSeconds
    , toSeconds', fromSeconds'

    -- * Lenses
    , _utcvDay, _utcvDayTime
    , _utctDay, _utctDayTime
    ) where

import Prelude
import Control.Lens
import Data.Thyme.Clock.Internal
import Data.Thyme.Clock.POSIX

-- | Get the current UTC time from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = fmap (review posixTime) getPOSIXTime

