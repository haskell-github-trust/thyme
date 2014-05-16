{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

#include "thyme.h"
#if HLINT
#include "cabal_macros.h"
#endif

module Data.Thyme.Clock.TAI
    ( AbsoluteTime
    , taiEpoch
    , LeapSecondTable
    , utcDayLength
    , absoluteTime
    , parseTAIUTCDAT
    ) where

import Prelude
import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.AffineSpace
import Data.Attoparsec.ByteString.Char8 ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Char
import Data.Data
import Data.Either
import Data.Ix
#if MIN_VERSION_containers(0,5,0)
import qualified Data.Map.Strict as Map
#else
import qualified Data.Map as Map
#endif
import Data.Thyme.Calendar
import Data.Thyme.Clock.Internal
import Data.Thyme.Format.Internal
import Data.Thyme.LocalTime
#if __GLASGOW_HASKELL__ == 704
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
#endif
import Data.Vector.Unboxed.Deriving
import Data.VectorSpace
import GHC.Generics (Generic)
import System.Locale
import System.Random (Random)
import Test.QuickCheck

newtype AbsoluteTime = AbsoluteTime DiffTime deriving (INSTANCES_MICRO)

derivingUnbox "AbsoluteTime" [t| AbsoluteTime -> DiffTime |]
    [| \ (AbsoluteTime a) -> a |] [| AbsoluteTime |]

instance Show AbsoluteTime where
    {-# INLINEABLE showsPrec #-}
    showsPrec p tai = showsPrec p lt . (++) " TAI" where
        lt = tai ^. from (absoluteTime (const zeroV)) . utcLocalTime utc

-- | The epoch of TAI, which is 1858-11-17 00:00:00 TAI.
{-# INLINE taiEpoch #-}
taiEpoch :: AbsoluteTime
taiEpoch = AbsoluteTime zeroV

instance AffineSpace AbsoluteTime where
    type Diff AbsoluteTime = DiffTime
    {-# INLINE (.-.) #-}
    (.-.) = \ (AbsoluteTime a) (AbsoluteTime b) -> a ^-^ b
    {-# INLINE (.+^) #-}
    (.+^) = \ (AbsoluteTime a) d -> AbsoluteTime (a ^+^ d)

type LeapSecondTable = Either UTCTime AbsoluteTime -> DiffTime

utcDayLength :: LeapSecondTable -> Day -> DiffTime
utcDayLength table day@((.+^ 1) -> next) =
        DiffTime posixDay ^+^ diff next ^-^ diff day where
    diff d = table . Left $ utcTime # UTCTime d zeroV
    NominalDiffTime posixDay = posixDayLength

{-# INLINE absoluteTime #-}
absoluteTime :: LeapSecondTable -> Iso' UTCTime AbsoluteTime
absoluteTime table = iso toTAI fromTAI where

    {-# INLINE toTAI #-}
    toTAI :: UTCTime -> AbsoluteTime
    toTAI ut@(UTCRep (NominalDiffTime u)) =
        AbsoluteTime (DiffTime u ^+^ table (Left ut))

    {-# INLINE fromTAI #-}
    fromTAI :: AbsoluteTime -> UTCTime
    fromTAI tai@(AbsoluteTime a) = UTCRep (NominalDiffTime u) where
        DiffTime u = a ^-^ table (Right tai)

-- | @tai-utc.dat@ from <http://maia.usno.navy.mil/ser7/tai-utc.dat>
{-# INLINEABLE parseTAIUTCDAT #-}
parseTAIUTCDAT :: ByteString -> LeapSecondTable
parseTAIUTCDAT = parse $ do
    y <- dec_ 5 <* P.skipSpace <?> "Year"
    let mons = map toUpper . snd <$> months defaultTimeLocale
    m <- succ <$> indexOf mons <* P.skipSpace <?> "Month"
    d <- dec_ 2 <?> "Day"
    tokens ["=", "JD"]
    -- TAI-UTC changes always happen at midnight, so just ignore ".5".
    mjd <- subtract 2400000{-.5-} <$> P.decimal
        <* P.string ".5" <?> "Julian Date .5"
    let ymd = YearMonthDay y m d
    unless (gregorian # ymd == ModifiedJulianDay mjd) . fail $
        show ymd ++ " is not Modified Julian Day " ++ show mjd

    tokens ["TAI", "-", "UTC", "="]
    b <- P.rational <?> "Base"
    tokens ["S", "+", "(", "MJD", "-"]
    o <- P.rational <?> "Offset"
    tokens [".", ")", "X"]
    c <- P.rational <* tokens ["S"] <?> "Coefficient"

    -- FIXME: confirm UTC↔TAI conversion for pre-1972.
    -- Do we round MJD? This is a guess:
    -- TAI-UTC =  b + c * (MJD(UTC) - o)
    let atUTC (UTCRep t) = fromSeconds' $ b + c * (toMJD t - o)
    -- TAI-UTC = (b + c * (MJD(TAI) - o)) / (1 + c)
    let atTAI (AbsoluteTime t) = fromSeconds' $ b + c * (toMJD t - o) / (1 + c)
    let NominalDiffTime ((toRational mjd *^) -> begin) = posixDayLength
    let beginUTC = UTCRep (NominalDiffTime begin)
    let beginTAI = AbsoluteTime (DiffTime begin ^-^ atUTC beginUTC)
    return ((beginUTC, atUTC), (beginTAI, atTAI))

  where
    toMJD t = toSeconds t / toSeconds posixDayLength
    tokens = foldr (\ tok a -> P.skipSpace >> P.string tok >> a) P.skipSpace

    parse row = pair . unzip . rights . map (P.parseOnly row) . S.lines
    pair (look -> atUTC, look -> atTAI) = either atUTC atTAI
#if MIN_VERSION_containers(0,5,0)
    look l = \ t -> maybe zeroV (($ t) . snd) $ Map.lookupLE t (Map.fromList l)
#else
    look l = \ t -> case Map.splitLookup t (Map.fromList l) of
        (lt, eq, _) -> maybe zeroV ($ t) $ eq <|> fst <$> Map.maxView lt
#endif

