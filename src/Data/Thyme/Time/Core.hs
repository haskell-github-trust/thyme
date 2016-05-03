{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

#if HLINT
#include "cabal_macros.h"
#endif

-- | This module provides the 'Thyme' typeclass, and instances for
-- converting between "Data.Time" and "Data.Thyme" types. It also provides
-- compatibility wrappers for existing code using "Data.Time".
--
-- Note that we do not provide 'Num' hierarchy instances for 'DiffTime' nor
-- 'NominalDiffTime' here. If you want to use them anyway despite parts of
-- them being ill-defined (e.g. @('*')@ on 'DiffTime'), import
-- "Data.Thyme.Time" instead.

module Data.Thyme.Time.Core
    ( module Data.Thyme
    , module Data.Thyme.Time.Core
    ) where

import Prelude
import Control.Lens
import Data.AffineSpace
import Data.Fixed
import Data.Ratio
import Data.Thyme
import Data.Thyme.Clock.Internal
import Data.Thyme.Clock.TAI
import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.TAI as T
import qualified Data.Time.LocalTime as T
import Unsafe.TrueName

------------------------------------------------------------------------
-- * Type conversion

-- | Typeclass for converting between "Data.Time" and "Data.Thyme" types.
class Thyme time thyme | thyme -> time where
    -- | Convert between "Data.Time" and "Data.Thyme" types.
    --
    -- @
    -- > :set -t
    -- > import qualified "Data.Time"
    --
    -- > 'thyme' 'Control.Lens.#' ('fromSeconds'' 10 :: 'DiffTime')
    -- 10s
    -- it :: 'Data.Time.DiffTime'
    --
    -- > 'Data.Time.secondsToDiffTime' 10 '^.' 'thyme' :: 'DiffTime'
    -- 10s
    -- it :: 'DiffTime'
    -- @
    thyme :: Iso' time thyme

instance Thyme T.Day Day where
    {-# INLINE thyme #-}
    thyme = iso
        (ModifiedJulianDay . fromInteger . T.toModifiedJulianDay)
        (T.ModifiedJulianDay . toInteger . toModifiedJulianDay)

instance Thyme T.UniversalTime UniversalTime where
    {-# INLINE thyme #-}
    thyme = iso T.getModJulianDate T.ModJulianDate . from modJulianDate

instance Thyme T.DiffTime DiffTime where
    {-# INLINE thyme #-}
    thyme = dt . fixed . from picoseconds where
        dt = iso (\ [truename| ''T.DiffTime MkDiffTime | ps |] -> ps )
            [truename| ''T.DiffTime MkDiffTime |]
#if MIN_VERSION_base(4,7,0)
        fixed = iso (\ (MkFixed n) -> n ) MkFixed
#else
        fixed = iso (\ [truename| ''Fixed MkFixed | n |] -> n )
            [truename| ''Fixed MkFixed |]
#endif

instance Thyme T.NominalDiffTime NominalDiffTime where
    {-# INLINE thyme #-}
    thyme = ndt . fixed . from picoseconds where
        ndt = iso (\ [truename| ''T.NominalDiffTime MkNominalDiffTime | ps |] -> ps )
            [truename| ''T.NominalDiffTime MkNominalDiffTime |]
#if MIN_VERSION_base(4,7,0)
        fixed = iso (\ (MkFixed n) -> n ) MkFixed
#else
        fixed = iso (\ [truename| ''Fixed MkFixed | n |] -> n )
            [truename| ''Fixed MkFixed |]
#endif

instance Thyme T.UTCTime UTCView where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.UTCTime d t) -> UTCView (d ^. thyme) (t ^. thyme))
        (\ (UTCView d t) -> T.UTCTime (thyme # d) (thyme # t))

instance Thyme T.UTCTime UTCTime where
    {-# INLINE thyme #-}
    thyme = thyme . from utcTime

instance Thyme T.AbsoluteTime AbsoluteTime where
    {-# INLINE thyme #-}
    thyme = iso (`T.diffAbsoluteTime` T.taiEpoch)
            (`T.addAbsoluteTime` T.taiEpoch)
        . thyme . iso (taiEpoch .+^) (.-. taiEpoch)

instance Thyme T.TimeZone TimeZone where
    {-# INLINE thyme #-}
    thyme = iso (\ T.TimeZone {..} -> TimeZone {..})
        (\ TimeZone {..} -> T.TimeZone {..})

instance Thyme T.TimeOfDay TimeOfDay where
    {-# INLINE thyme #-}
    thyme = iso ( \ (T.TimeOfDay h m s) -> TimeOfDay h m $
            microseconds # round (s * 1000000) )
        ( \ (TimeOfDay h m s) -> T.TimeOfDay h m . fromRational $
            toInteger (s ^. microseconds) % 1000000 )

instance Thyme T.LocalTime LocalTime where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.LocalTime d t) -> LocalTime (d ^. thyme) (t ^. thyme))
        (\ (LocalTime d t) -> T.LocalTime (thyme # d) (thyme # t))

instance Thyme T.ZonedTime ZonedTime where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.ZonedTime t z) -> ZonedTime (t ^. thyme) (z ^. thyme))
        (\ (ZonedTime t z) -> T.ZonedTime (thyme # t) (thyme # z))

-- | Convert a "Data.Time" type to a "Data.Thyme" type, if you would rather
-- not use "Control.Lens" directly.
--
-- @
-- 'toThyme' = 'view' 'thyme'
-- 'toThyme' t ≡ t '^.' 'thyme'
-- @
{-# INLINE toThyme #-}
toThyme :: (Thyme time thyme) => time -> thyme
toThyme = view thyme

-- | Convert a "Data.Thyme" type to a "Data.Time" type, if you would rather
-- not use "Control.Lens" directly.
--
-- @
-- 'fromThyme' = 'review' 'thyme'
-- 'fromThyme' t ≡ 'thyme' 'Control.Lens.#' t
-- @
{-# INLINE fromThyme #-}
fromThyme :: (Thyme time thyme) => thyme -> time
fromThyme = review thyme

