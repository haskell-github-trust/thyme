{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-} -- workaround
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- #hide
module Data.Thyme.Clock.Internal where

import Prelude
import Control.DeepSeq
import Control.Lens
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Basis
import Data.Data
import Data.Int
import Data.Ix
import Data.Micro
import Data.Thyme.Calendar
import Data.VectorSpace

#if !SHOW_INTERNAL
import Control.Monad
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (char)
import Text.Read (readPrec)
#endif

-- | Workaround for GHC refusing to match RULES for functions with equality
-- constraints; see <http://hackage.haskell.org/trac/ghc/ticket/7611>.
class (HasBasis t, Basis t ~ ()) => OneDimensional t
instance OneDimensional DiffTime
instance OneDimensional NominalDiffTime

-- | Time interval as a 'Rational' number of seconds. Compose with 'simple'
-- or 'simply' 'view' / 'review' to avoid ambiguous type variables.
{-# INLINE seconds #-}
seconds :: (HasBasis s, Basis s ~ (), HasBasis t, Basis t ~ ()) => Iso s t (Scalar s) (Scalar t)
seconds = iso (`decompose'` ()) (*^ basisValue ())

-- | Convert a time interval to some 'Fractional' type.
--
-- @
-- toSeconds :: (HasBasis s, Basis s ~ (), Scalar s ~ a, Real a, Fractional n) => s -> n
-- @
{-# INLINE toSeconds #-}
toSeconds :: (OneDimensional s, Real (Scalar s), Fractional n) => s -> n
toSeconds = realToFrac . simply view seconds

-- | Make a time interval from some 'Real' type. 'Rational'-avoiding rewrite
-- rules included for 'Double', 'Float', 'Integer', 'Int' and 'Int64'.
--
-- @
-- fromSeconds :: (HasBasis t, Basis t ~ (), Scalar t ~ b, Real n, Fractional b) => n -> t
-- @
{-# INLINE fromSeconds #-}
fromSeconds :: (OneDimensional t, Real n, Fractional (Scalar t)) => n -> t
fromSeconds = simply review seconds . realToFrac

-- | Type-restricted 'toSeconds' to avoid constraint-defaulting warnings.
{-# INLINE toSeconds' #-}
toSeconds' :: (HasBasis s, Basis s ~ ()) => s -> Scalar s
toSeconds' = simply view seconds

-- | Type-restricted 'fromSeconds' to avoid constraint-defaulting warnings.
{-# INLINE fromSeconds' #-}
fromSeconds' :: (HasBasis t, Basis t ~ ()) => Scalar t -> t
fromSeconds' = simply review seconds

{-# RULES

"toSeconds∷DiffTime→Fractional"
    toSeconds = (/ 1000000) . fromIntegral . review microDiffTime
"toSeconds∷NominalDiffTime→Fractional"
    toSeconds = (/ 1000000) . fromIntegral . review microNominalDiffTime

"fromSeconds∷Double→DiffTime"
    fromSeconds = view microDiffTime . round . (*) (1000000 :: Double)
"fromSeconds∷Double→NominalDiffTime"
    fromSeconds = view microNominalDiffTime . round . (*) (1000000 :: Double)

"fromSeconds∷Float→DiffTime"
    fromSeconds = view microDiffTime . round . (*) (1000000 :: Float)
"fromSeconds∷Float→NominalDiffTime"
    fromSeconds = view microNominalDiffTime . round . (*) (1000000 :: Float)

"fromSeconds∷Integer→{,Nominal}DiffTime"
    fromSeconds = fromSeconds . (fromInteger :: Integer -> Int64)
"fromSeconds∷Int→{,Nominal}DiffTime"
    fromSeconds = fromSeconds . (fromIntegral :: Int -> Int64)

"fromSeconds∷Int64→DiffTime"
    fromSeconds = view microDiffTime . (*) 1000000
"fromSeconds∷Int64→NominalDiffTime"
    fromSeconds = view microNominalDiffTime . (*) 1000000 #-}

------------------------------------------------------------------------

newtype DiffTime = DiffTime Micro
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable, AdditiveGroup)

#if SHOW_INTERNAL
deriving instance Show DiffTime
deriving instance Read DiffTime
#else
instance Show DiffTime where
    {-# INLINEABLE showsPrec #-}
    showsPrec p (DiffTime a) = showsPrec p a . (:) 's'
instance Read DiffTime where
    {-# INLINEABLE readPrec #-}
    readPrec = return (const . DiffTime) `ap` readPrec `ap` lift (char 's')
#endif

instance VectorSpace DiffTime where
    type Scalar DiffTime = Rational
    {-# INLINE (*^) #-}
    s *^ DiffTime t = DiffTime (s *^ t)

instance HasBasis DiffTime where
    type Basis DiffTime = ()
    {-# INLINE basisValue #-}
    basisValue () = DiffTime (basisValue ())
    {-# INLINE decompose #-}
    decompose (DiffTime a) = decompose a
    {-# INLINE decompose' #-}
    decompose' (DiffTime a) = decompose' a

{-# INLINE microDiffTime #-}
microDiffTime :: Iso' Int64 DiffTime
microDiffTime = iso (DiffTime . Micro) (\ (DiffTime (Micro u)) -> u)

------------------------------------------------------------------------

newtype NominalDiffTime = NominalDiffTime Micro
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable, AdditiveGroup)

#if SHOW_INTERNAL
deriving instance Show NominalDiffTime
deriving instance Read NominalDiffTime
#else
instance Show NominalDiffTime where
    {-# INLINEABLE showsPrec #-}
    showsPrec p (NominalDiffTime a) rest = showsPrec p a ('s' : rest)
instance Read NominalDiffTime where
    {-# INLINEABLE readPrec #-}
    readPrec = return (const . NominalDiffTime) `ap` readPrec `ap` lift (char 's')
#endif

instance VectorSpace NominalDiffTime where
    type Scalar NominalDiffTime = Rational
    {-# INLINE (*^) #-}
    s *^ NominalDiffTime t = NominalDiffTime (s *^ t)

instance HasBasis NominalDiffTime where
    type Basis NominalDiffTime = ()
    {-# INLINE basisValue #-}
    basisValue () = NominalDiffTime (basisValue ())
    {-# INLINE decompose #-}
    decompose (NominalDiffTime a) = decompose a
    {-# INLINE decompose' #-}
    decompose' (NominalDiffTime a) = decompose' a

{-# INLINE microNominalDiffTime #-}
microNominalDiffTime :: Iso' Int64 NominalDiffTime
microNominalDiffTime = iso (NominalDiffTime . Micro)
    (\ (NominalDiffTime (Micro u)) -> u)

{-# INLINE posixDayLength #-}
posixDayLength :: NominalDiffTime
posixDayLength = NominalDiffTime (toMicro 86400)

------------------------------------------------------------------------

newtype UniversalTime = UniversalRep NominalDiffTime -- since MJD epoch
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable)

{-# INLINE modJulianDate #-}
modJulianDate :: Iso' UniversalTime Rational
modJulianDate = iso ( \ (UniversalRep t) ->
        simply view seconds t / simply view seconds posixDayLength )
    (UniversalRep . (*^ posixDayLength))

------------------------------------------------------------------------

newtype UTCTime = UTCRep NominalDiffTime -- since MJD epoch
    deriving (Eq, Ord, Enum, Ix, Bounded, NFData, Data, Typeable)

data UTCView = UTCTime
    { utctDay :: {-# UNPACK #-}!Day
    , utctDayTime :: {-# UNPACK #-}!DiffTime
    } deriving (Eq, Ord, Data, Typeable, Show)

instance NFData UTCView

_utctDay :: Lens' UTCTime Day
_utctDay = utcTime . lens utctDay (\ (UTCTime _ t) d -> UTCTime d t)

_utctDayTime :: Lens' UTCTime DiffTime
_utctDayTime = utcTime . lens utctDayTime (\ (UTCTime d _) t -> UTCTime d t)

instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    {-# INLINE (.-.) #-}
    UTCRep a .-. UTCRep b = a ^-^ b
    {-# INLINE (.+^) #-}
    UTCRep a .+^ d = UTCRep (a ^+^ d)

{-# INLINE utcTime #-}
utcTime :: Iso' UTCTime UTCView
utcTime = iso toView fromView where
    NominalDiffTime posixDay@(Micro uPosixDay) = posixDayLength

    {-# INLINE toView #-}
    toView :: UTCTime -> UTCView
    toView (UTCRep (NominalDiffTime a)) = UTCTime
            (ModifiedJulianDay mjd) (DiffTime dt) where
        (fromIntegral -> mjd, dt) = microDivMod a posixDay

    {-# INLINE fromView #-}
    fromView :: UTCView -> UTCTime
    fromView (UTCTime (ModifiedJulianDay mjd) (DiffTime dt)) = UTCRep a where
        a = NominalDiffTime (Micro (fromIntegral mjd * uPosixDay) ^+^ dt)

