module Data.Thyme.Format.Internal where

import Prelude
import Data.Int

{-# INLINE shows02 #-}
shows02 :: Int -> String -> String
shows02 n = if n < 10 then (:) '0' . shows n else shows n

{-# INLINE shows_2 #-}
shows_2 :: Int -> String -> String
shows_2 n = if n < 10 then (:) ' ' . shows n else shows n

{-# INLINE shows03 #-}
shows03 :: Int -> ShowS
shows03 n
    | n < 10 = (++) "00" . shows n
    | n < 100 = (++) "0" . shows n
    | otherwise = shows n

{-# INLINE shows04 #-}
shows04 :: Int -> String -> String
shows04 n
    | n < 10 = (++) "000" . shows n
    | n < 100 = (++) "00" . shows n
    | n < 1000 = (++) "0" . shows n
    | otherwise = shows n

{-# INLINE fills06 #-}
fills06 :: Int64 -> ShowS
fills06 n
    | n < 10 = (++) "00000"
    | n < 100 = (++) "0000"
    | n < 1000 = (++) "000"
    | n < 10000 = (++) "00"
    | n < 100000 = (++) "0"
    | otherwise = id

{-# INLINE drops0 #-}
drops0 :: Int64 -> ShowS
drops0 n = case divMod n 10 of
    (q, 0) -> drops0 q
    _ -> shows n

