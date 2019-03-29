{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#include "thyme.h"

-- | FOR INTERNAL USE ONLY.
module Data.Thyme.Internal.Micro where

import Prelude
import Control.DeepSeq
import Data.AdditiveGroup
import Data.Basis
import Data.Data
import Data.Hashable
import Data.Int
import Data.Ix
import Data.Ratio
#if __GLASGOW_HASKELL__ == 704
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
#endif
import Data.Vector.Unboxed.Deriving
import Data.VectorSpace
import GHC.Generics (Generic)
import System.Random
#ifdef QUICKCHECK
import Test.QuickCheck
#endif

#if !SHOW_INTERNAL
import Control.Monad
import Data.Char
import Data.Thyme.Format.Internal
import Numeric
import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP
import Text.Read
#endif

newtype Micro = Micro Int64 deriving (INSTANCES_MICRO)

derivingUnbox "Micro" [t| Micro -> Int64 |]
    [| \ (Micro a) -> a |] [| Micro |]

#if SHOW_INTERNAL
deriving instance Show Micro
deriving instance Read Micro
#else
instance Show Micro where
    {-# INLINEABLE showsPrec #-}
    showsPrec _ (Micro a) = sign . shows si . frac where
        sign = if a < 0 then (:) '-' else id
        (si, su) = abs a `divMod` 1000000
        frac = if su == 0 then id else (:) '.' . fills06 su . drops0 su

instance Read Micro where
    {-# INLINEABLE readPrec #-}
    readPrec = lift $ do
        sign <- (char '-' >> return negate) `mplus` return id
        s <- readS_to_P readDec
        us <- (`mplus` return 0) $ do
            _ <- char '.'
            [(us10, "")] <- (readDec . take 7 . (++ "000000"))
                `fmap` munch1 isDigit
            return (div (us10 + 5) 10)
        return . Micro . sign $ s * 1000000 + us
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
    (^+^) = \ (Micro a) (Micro b) -> Micro (a + b)
    {-# INLINE negateV #-}
    negateV = \ (Micro a) -> Micro (negate a)

instance VectorSpace Micro where
    type Scalar Micro = Rational
    {-# INLINE (*^) #-}
    s *^ Micro a = Micro . fromInteger $ -- 'round'-to-even
        case compare (2 * abs r) (denominator s) of
            LT -> n
            EQ -> if even n then n else m
            GT -> m
      where
        (n, r) = quotRem (toInteger a * numerator s) (denominator s)
        m = if r < 0 then n - 1 else n + 1

instance HasBasis Micro where
    type Basis Micro = ()
    {-# INLINE basisValue #-}
    basisValue = \ _ -> Micro 1000000
    {-# INLINE decompose #-}
    decompose = \ (Micro a) -> [((), fromIntegral a % 1000000)]
    {-# INLINE decompose' #-}
    decompose' = \ (Micro a) _ -> fromIntegral a % 1000000

