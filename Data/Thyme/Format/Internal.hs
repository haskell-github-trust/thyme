module Data.Thyme.Format.Internal where

import Prelude

{-# INLINE shows02 #-}
shows02 :: Int -> String -> String
shows02 n = if n < 10 then (:) '0' . shows n else shows n

{-# INLINE shows04 #-}
shows04 :: Int -> String -> String
shows04 n = case () of
    _ | n < 10 -> (:) '0' . (:) '0' . (:) '0' . shows n
    _ | n < 100 -> (:) '0' . (:) '0' . shows n
    _ | n < 1000 -> (:) '0' . shows n
    _ -> shows n

