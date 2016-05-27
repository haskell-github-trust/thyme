{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Thyme.Format.DateFast (
    parseFastUtc
) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Lens        (from, view)
import           Control.Monad       (unless, void)
import qualified Data.ByteString     as BS
import           Data.Int            (Int64)
import           Data.List           (foldl1')
import qualified Data.Text           as T
import           Data.Text.Encoding  (encodeUtf8)
import           Data.Thyme.Calendar (fromGregorian)
import           Data.Thyme.Clock
import           Data.Word           (Word8)
import           Scanner             (Scanner)
import qualified Scanner             as S

satisfy :: (Word8 -> Bool) -> Scanner Word8
satisfy f = do
  c <- S.anyWord8
  unless (f c) $ fail $ "Unexpected " ++ show c
  return c
{-# INLINE satisfy #-}

satisfyChar :: (Char -> Bool) -> Scanner Char
satisfyChar f = do
  c <- S.anyChar8
  unless (f c) $ fail $ "Unexpected " ++ show c
  return c
{-# INLINE satisfyChar #-}

digit :: Scanner Int
digit = do
  c <- satisfy (\c -> c >= 48 && c <= 57)
  return $ fromIntegral c - 48
{-# INLINE digit #-}

many1digit :: Scanner [Int]
many1digit = do
  start <- digit
  rest <- S.takeWhile (\c -> c >=48 && c <= 57)
  let nums = map (fromIntegral . subtract 48) $ BS.unpack rest
  return (start : nums)
{-# INLINE many1digit #-}

-- | Parse integer number read up to maxdigits; stop if different character is found
parseNumber2 :: Scanner Int
parseNumber2 = do -- Specialized version for 2 digits
  c1 <- digit
  c2 <- digit
  return (10 * c1 + c2)
{-# INLINE parseNumber2 #-}

parseNumber4 :: Scanner Int
parseNumber4 = do -- Specialized version for 2 digits
  c1 <- digit
  c2 <- digit
  c3 <- digit
  c4 <- digit
  return (1000 * c1 + 100 * c2 + 10 * c3 + c4)
{-# INLINE parseNumber4 #-}

toffset :: Scanner Int64
toffset = do
  hours <- parseNumber2
  S.char8 ':'
  minutes <- parseNumber2
  return $ fromIntegral $ hours * 3600 + minutes * 60
{-# INLINE toffset #-}


parserRfc :: Scanner UTCTime
parserRfc = do
    year <- parseNumber4
    S.char8 '-'
    month <- parseNumber2
    S.char8 '-'
    dayofmonth <- parseNumber2
    S.char8 'T'
    hour <- fromIntegral <$> parseNumber2
    S.char8 ':'
    minute <- fromIntegral <$> parseNumber2
    S.char8 ':'
    seconds <- fromIntegral <$> parseNumber2
    dot <- S.lookAheadChar8
    micros <- case dot of
        Just '.' -> do
            void S.anyChar8
            numlst <- take 6 <$> many1digit
            let num = foldl1' (\a b -> 10 * a + b) numlst
            return $ fromIntegral $ num * (10 ^ (6 - length numlst))
        Just _ -> return 0
        Nothing -> fail "Not enough input"
    zone <- satisfyChar (\c -> c == '+' || c == '-' || c == 'Z')
    offset <- case zone of
          'Z' -> return 0
          '+' -> toffset
          '-' -> negate <$> toffset
          _ -> fail "Expected Z/+/- while parsing date."
    let totalMicro = micros + 1000000 * seconds + 1000000 * 60 * minute + 1000000 * 3600 * hour
                     - offset * 1000000 :: Int64
        tdiff = view (from microseconds) totalMicro
        tday = fromGregorian year month dayofmonth
    return $ view (from utcTime) (UTCView tday tdiff)

parseFastUtc :: Monad m => T.Text -> m UTCTime
parseFastUtc t =
  case S.scanOnly parserRfc (encodeUtf8 t) of
        Right d -> return d
        Left err -> fail $ "could not parse ISO-8601 date: " ++ err
