{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides compatibility instances and wrappers for the
-- things that @thyme@ does differently from @time@, and allows it to be
-- used as a drop-in replacement for the latter, with the exceptions noted
-- below:
--
--   * When constructing an 'UTCTime' or 'UniversalTime', use 'mkUTCTime' or
--   'mkModJulianDate' in place of @UTCTime@ or @ModJulianDate@.
--
--   * Instead of pattern matching on @UTCTime@, use 'unUTCTime' to get
--   a 'UTCView', which has a constructor @UTCTime@ with the same fields.
--   For @ModJulianDate@, use 'getModJulianDate'. @ViewPatterns@ may make
--   the transition more seamless.
--
--   * Where a third party library uses @time@, you can use 'toThyme' and
--   'fromThyme' to convert between the corresponding types.
--
--   * 'Year's are 'Int's, not 'Integer's: you may need 'fromIntegral'.
--
-- You shouldn't need to use @lens@ or @vector-space@ directly if you don't
-- want to. However if you do use @vector-space@ and wish to avoid the
-- 'RealFrac' instances for 'DiffTime' and 'NominalDiffTime', import
-- "Data.Thyme.Time.Core" instead.
--
-- Anything else is probably not intentional, and you should either contact
-- me via IRC or file an issue at <https://github.com/liyang/thyme/issues>.

module Data.Thyme.Time
    ( module Data.Thyme.Time.Core
    {- instance RealFrac {,Nominal}DiffTime -}
    ) where

import Prelude
import Data.Thyme.Internal.Micro
import Data.Ratio
import Data.Thyme
import Data.Thyme.Clock.Internal
import Data.Thyme.Time.Core

instance Num Micro where
    {-# INLINE (+) #-}
    {-# INLINE (-) #-}
    {-# INLINE (*) #-}
    {-# INLINE negate #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}
    Micro a + Micro b = Micro (a + b)
    Micro a - Micro b = Micro (a - b)
    Micro a * Micro b = Micro (quot a 1000 * quot b 1000)
    negate (Micro a) = Micro (negate a)
    abs (Micro a) = Micro (abs a)
    signum (Micro a) = Micro (signum a * 1000000)
    fromInteger a = Micro (fromInteger a * 1000000)

instance Real Micro where
    {-# INLINE toRational #-}
    toRational (Micro a) = toInteger a % 1000000

instance Fractional Micro where
    {-# INLINE (/) #-}
    {-# INLINE recip #-}
    {-# INLINE fromRational #-}
    Micro a / Micro b = Micro (quot (a * 1000) (b `quot` 1000))
    recip (Micro a) = Micro (quot 1000000 a)
    fromRational = toMicro

instance RealFrac Micro where
    {-# INLINE properFraction #-}
    properFraction a = (fromIntegral q, r) where
        (q, r) = microQuotRem a (Micro 1000000)

deriving instance Num DiffTime
deriving instance Real DiffTime
deriving instance Fractional DiffTime
deriving instance RealFrac DiffTime

deriving instance Num NominalDiffTime
deriving instance Real NominalDiffTime
deriving instance Fractional NominalDiffTime
deriving instance RealFrac NominalDiffTime

{-# RULES

"realToFrac/DiffTime-NominalDiffTime"
    realToFrac = \ (DiffTime d) -> NominalDiffTime d
"realToFrac/NominalDiffTime-DiffTime"
    realToFrac = \ (NominalDiffTime d) -> DiffTime d

"realToFrac/DiffTime-Fractional"
    realToFrac = toSeconds :: (Fractional n) => DiffTime -> n
"realToFrac/NominalDiffTime-Fractional"
    realToFrac = toSeconds :: (Fractional n) => NominalDiffTime -> n

"realToFrac/Real-DiffTime"
    realToFrac = fromSeconds :: (Real n) => n -> DiffTime
"realToFrac/Real-NominalDiffTime"
    realToFrac = fromSeconds :: (Real n) => n -> NominalDiffTime #-}

