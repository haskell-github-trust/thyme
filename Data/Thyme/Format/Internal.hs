module Data.Thyme.Format.Internal where

import Prelude
import Data.Micro

{-# INLINE shows02 #-}
shows02 :: Int -> String -> String
shows02 n = if n < 10 then (:) '0' . shows n else shows n

{-# INLINE show02 #-}
show02 :: Int -> String
show02 n = if n < 10 then '0' : show n else show n

{-# INLINE show_2 #-}
show_2 :: Int -> String
show_2 n = if n < 10 then ' ' : show n else show n

{-# INLINE show03 #-}
show03 :: Int -> String
show03 n = case () of
    _ | n < 10 -> '0' : '0' : show n
    _ | n < 100 -> '0' : show n
    _ -> show n

{-# INLINE shows04 #-}
shows04 :: Int -> String -> String
shows04 n = case () of
    _ | n < 10 -> (:) '0' . (:) '0' . (:) '0' . shows n
    _ | n < 100 -> (:) '0' . (:) '0' . shows n
    _ | n < 1000 -> (:) '0' . shows n
    _ -> shows n

{-# INLINE show04 #-}
show04 :: Int -> String
show04 n = shows04 n ""

{-# INLINE show60 #-}
show60 :: Micro -> String
show60 (Micro u) = case () of
    _ | u < 10 -> '0' : '0' : '0' : '0' : '0' : show u
    _ | u < 100 -> '0' : '0' : '0' : '0' : show u
    _ | u < 1000 -> '0' : '0' : '0' : show u
    _ | u < 10000 -> '0' : '0' : show u
    _ | u < 100000 -> '0' : show u
    _ -> show u

{-# INLINE show6 #-}
show6 :: Micro -> String
show6 (Micro u) = case () of
    _ | u == 0 -> ""
    _ | u < 10 -> '.' : '0' : '0' : '0' : '0' : '0' : shrink u
    _ | u < 100 -> '.' : '0' : '0' : '0' : '0' : shrink u
    _ | u < 1000 -> '.' : '0' : '0' : '0' : shrink u
    _ | u < 10000 -> '.' : '0' : '0' : shrink u
    _ | u < 100000 -> '.' : '0' : shrink u
    _ -> '.' : shrink u
  where
    shrink v = case divMod v 10 of
        (w, 0) -> shrink w
        _ -> show v

