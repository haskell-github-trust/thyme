module Data.Thyme.Format.Internal where

import Prelude

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

