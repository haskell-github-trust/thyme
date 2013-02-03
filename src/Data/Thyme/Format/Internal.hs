{-# LANGUAGE CPP #-}

module Data.Thyme.Format.Internal where

import Prelude
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
#if MIN_VERSION_bytestring(0,10,2)
import qualified Data.ByteString.Builder as S
#else
import qualified Data.ByteString.Lazy.Builder as S
#endif
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Data.Char
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

------------------------------------------------------------------------

{-# INLINE indexOf #-}
indexOf :: [String] -> Parser Int
indexOf = P.choice . zipWith (\ i s -> i <$ P.string (S.pack s)) [0..]

{-# INLINE indexOfCI #-}
indexOfCI :: [String] -> Parser Int
indexOfCI = P.choice . zipWith (\ i s -> i <$ stringCI s) [0..]

-- | Case-insensitive UTF-8 ByteString parser
--
-- Matches one character at a time. Slow.
{-# INLINE stringCI #-}
stringCI :: String -> Parser ()
stringCI = foldl (\ p c -> p *> charCI c) (pure ())

-- | Case-insensitive UTF-8 ByteString parser
--
-- We can't easily perform upper/lower case conversion on the input, so
-- instead we accept either one of @toUpper c@ and @toLower c@.
{-# INLINE charCI #-}
charCI :: Char -> Parser ()
charCI c = if u == l then charU8 c else charU8 l <|> charU8 u where
    l = toLower c
    u = toUpper c

{-# INLINE charU8 #-}
charU8 :: Char -> Parser ()
charU8 c = () <$ P.string (L.toStrict . S.toLazyByteString . S.charUtf8 $ c)

-- | Number may be prefixed with '-'
{-# INLINE negative #-}
negative :: Parser Int64 -> Parser Int64
negative p = ($) <$> (negate <$ P.char '-' <|> pure id) <*> p

-- | Fixed-length 0-padded decimal
{-# INLINE dec0 #-}
dec0 :: Int -> Parser Int
dec0 n = either fail return . P.parseOnly P.decimal =<< P.take n

-- | Fixed-length space-padded decimal
{-# INLINE dec_ #-}
dec_ :: Int -> Parser Int
dec_ n = either fail return . P.parseOnly P.decimal
    =<< S.dropWhile isSpace <$> P.take n

