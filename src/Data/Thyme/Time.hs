{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides 'Num', 'Real', 'Fractional', and 'RealFrac'
-- instances for 'DiffTime' and 'NominalDiffTime'.
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
    fromRational r = Micro (round $ r * 1000000)

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

