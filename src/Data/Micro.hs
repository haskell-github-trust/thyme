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
import Data.VectorSpace

#if !SHOW_INTERNAL
import Control.Monad
import Data.Char
import Data.Thyme.Format.Internal
import Numeric
import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP
import Text.Read
#endif

newtype Micro = Micro Int64
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable)

#if SHOW_INTERNAL
deriving instance Show Micro
deriving instance Read Micro
#else
instance Show Micro where
    showsPrec _ (Micro a) = sign . shows si . frac where
        sign = if a < 0 then (:) '-' else id
        (si, su) = quotRem (abs a) 1000000
        frac = if su == 0 then id else (:) '.' . fills06 su . drops0 su

instance Read Micro where
    readPrec = lift $ do
        sign <- char '-' >> return negate `mplus` return id
        s <- readS_to_P readDec
        us <- (`mplus` return 0) $ do
            _ <- char '.'
            [(us10, "")] <- (readDec . take 7 . (++ "000000"))
                `fmap` munch1 isDigit
            return (div (us10 + 5) 10)
        return . Micro . sign $ s * 1000000 + us
#endif

{-# INLINE toMicro #-}
toMicro :: Rational -> Micro
toMicro r = Micro (fromInteger $ 1000000 * numerator r `div` denominator r)

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

