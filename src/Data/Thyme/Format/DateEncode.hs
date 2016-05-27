{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copied and adapted from aeson
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2013 Simon Meier <iridcode@gmail.com>
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>

module Data.Thyme.Format.DateEncode
  (
      utcTimeBuilder
    , quote
  ) where

import Control.Lens (view)
import Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim as BP
import Data.Char (chr)
import Data.Monoid ((<>))
import Data.Thyme.Clock
import Data.Thyme.Calendar

-- | Add quotes surrounding a builder
quote :: Builder -> Builder
quote b = B.char8 '"' <> b <> B.char8 '"'

ascii4 :: (Char, (Char, (Char, Char))) -> BP.BoundedPrim a
ascii4 cs = BP.liftFixedToBounded $ (const cs) >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii4 #-}

ascii6 :: (Char, (Char, (Char, (Char, (Char, Char))))) -> BP.BoundedPrim a
ascii6 cs = BP.liftFixedToBounded $ (const cs) >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii6 #-}

ascii8 :: (Char, (Char, (Char, (Char, (Char, (Char, (Char, Char)))))))
       -> BP.BoundedPrim a
ascii8 cs = BP.liftFixedToBounded $ (const cs) >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii8 #-}

day :: Day -> Builder
day dd = encodeYear yr <>
         BP.primBounded (ascii6 ('-',(mh,(ml,('-',(dh,dl)))))) ()
  where (yr,m,d)    = toGregorian dd
        !(T mh ml)  = twoDigits m
        !(T dh dl)  = twoDigits d
        encodeYear y
            | y >= 1000 = B.intDec y
            | y > 0 =
                let (ab,c) =  y `quotRem` 10
                    (a,b)  = ab `quotRem` 10
                in BP.primBounded (ascii4 ('0',(digit a,(digit b,digit c)))) ()
            | otherwise =
                error "Data.Aeson.Encode.Builder.day:  years BCE not supported"
{-# INLINE day #-}

timeOfDay64 :: DiffTime -> Builder
timeOfDay64 nom
  | frac == 0 = hhmmss -- omit subseconds if 0
  | otherwise = hhmmss <> BP.primBounded showFrac frac
  where
    micros = toMicroseconds nom
    (h, m') = micros `quotRem` (3600 * micro)
    (m, s) = m' `quotRem` (60 * micro)

    hhmmss  = BP.primBounded (ascii8 (hh,(hl,(':',(mh,(ml,(':',(sh,sl)))))))) ()
    !(T hh hl)  = twoDigits (fromIntegral h)
    !(T mh ml)  = twoDigits (fromIntegral m)
    !(T sh sl)  = twoDigits (fromIntegral real)
    (real,frac) = s `quotRem` micro
    showFrac = (\x -> ('.', x)) >$< (BP.liftFixedToBounded BP.char7 >*< trunc6)
    trunc6  = ((`quotRem` milli) . fromIntegral) >$<
              BP.condB (\(_,y) -> y == 0) (fst >$< trunc3) (digits3 >*< trunc3)
    digits3 = (`quotRem` 10) >$< (digits2 >*< digits1)
    digits2 = (`quotRem` 10) >$< (digits1 >*< digits1)
    digits1 = BP.liftFixedToBounded (digit >$< BP.char7)
    trunc3  = BP.condB (== 0) BP.emptyB $
              (`quotRem` 100) >$< (digits1 >*< trunc2)
    trunc2  = BP.condB (== 0) BP.emptyB $
              (`quotRem` 10)  >$< (digits1 >*< trunc1)
    trunc1  = BP.condB (== 0) BP.emptyB digits1

    micro      =       1000000 -- number of microseconds in 1 second
    milli      =          1000 -- number of milliseconds in 1 second
{-# INLINE timeOfDay64 #-}

dayTime :: Day -> DiffTime -> Builder
dayTime d t = day d <> B.char7 'T' <> timeOfDay64 t
{-# INLINE dayTime #-}

utcTimeBuilder :: UTCTime -> B.Builder
utcTimeBuilder utc = dayTime d s <> B.char7 'Z'
  where
    UTCView d s = view utcTime utc
{-# INLINE utcTimeBuilder #-}

data T = T {-# UNPACK #-} !Char {-# UNPACK #-} !Char

twoDigits :: Int -> T
twoDigits a     = T (digit hi) (digit lo)
  where (hi,lo) = a `quotRem` 10

digit :: Int -> Char
digit x = chr (x + 48)
