{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- #hide
module Data.Micro where

import Prelude
import Control.DeepSeq
import Data.AdditiveGroup
import Data.Basis
import Data.Data
import Data.Int
import Data.Ix
import Data.Ratio
#if !SHOW_INTERNAL
import Data.Thyme.Format.Internal
#endif
import Data.VectorSpace

newtype Micro = Micro Int64
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable)

#if SHOW_INTERNAL
deriving instance Show Micro
#else
instance Show Micro where
    showsPrec _ (Micro a) = sign . shows si . frac where
        sign = if a < 0 then (:) '-' else id
        (si, su) = quotRem (abs a) 1000000
        frac = if su == 0 then id else (:) '.' . fills06 su . drops0 su
#endif

{-# INLINE toMicro #-}
toMicro :: Rational -> Micro
toMicro r = Micro (fromInteger $ 1000000 * numerator r `div` denominator r)

#if INSTANCE_NUM
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
    Micro a / Micro b = Micro (quot (a * 1000) (b * 1000))
    recip (Micro a) = Micro (quot 1000000 a)
    fromRational = toMicro

instance RealFrac Micro where
    {-# INLINE properFraction #-}
    properFraction a = (fromIntegral q, r) where
        (q, r) = microQuotRem a (Micro 1000000)
#endif

{-# INLINE microQuotRem #-}
{-# INLINE microDivMod #-}
microQuotRem, microDivMod :: Micro -> Micro -> (Int64, Micro)
microQuotRem (Micro a) (Micro b) = (n, Micro f) where (n, f) = quotRem a b
microDivMod  (Micro a) (Micro b) = (n, Micro f) where (n, f) = divMod a b

instance AdditiveGroup Micro where
    {-# INLINE zeroV #-}
    zeroV = Micro 0
    {-# INLINE (^+^) #-}
    Micro a ^+^ Micro b = Micro (a + b)
    {-# INLINE negateV #-}
    negateV (Micro a) = Micro (negate a)

instance VectorSpace Micro where
    type Scalar Micro = Rational
    {-# INLINE (*^) #-}
    s *^ Micro a = Micro . fromInteger $
        toInteger a * numerator s `quot` denominator s

instance HasBasis Micro where
    type Basis Micro = ()
    {-# INLINE basisValue #-}
    basisValue () = Micro 1000000
    {-# INLINE decompose #-}
    decompose (Micro a) = [((), fromIntegral a % 1000000)]
    {-# INLINE decompose' #-}
    decompose' (Micro a) = const (fromIntegral a % 1000000)

{-# INLINE (^/^) #-}
(^/^) :: (HasBasis v, Basis v ~ (), Scalar v ~ s, Fractional s) => v -> v -> s
x ^/^ y = decompose' x () / decompose' y ()

