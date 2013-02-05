{-# LANGUAGE MultiParamTypeClasses #-}

-- | Compatibility between thyme and time.
module Data.Thyme.Time where

import Control.Lens
import Data.AffineSpace
import Data.Basis
import Data.Micro
import Data.Thyme.Calendar
import Data.Thyme.Clock.Internal
import Data.Thyme.Clock.TAI
import Data.Thyme.LocalTime
import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.TAI as T
import qualified Data.Time.LocalTime as T

class Thyme a b where
    thyme :: Simple Iso a b

instance Thyme T.Day Day where
    {-# INLINE thyme #-}
    thyme = iso
        (ModifiedJulianDay . fromInteger . T.toModifiedJulianDay)
        (T.ModifiedJulianDay . fromIntegral . toModifiedJulianDay)

instance Thyme T.UniversalTime UniversalTime where
    {-# INLINE thyme #-}
    thyme = iso T.getModJulianDate T.ModJulianDate . from modJulianDate

instance Thyme T.DiffTime DiffTime where
    {-# INLINE thyme #-}
    thyme = iso (microsecondsToDiffTime . round . (*) 1000000)
        ( \ (DiffTime (Micro t)) ->
            T.picosecondsToDiffTime $ toInteger t * 1000000 )

instance Thyme T.UTCTime UTCView where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.UTCTime d t) -> UTCTime (view thyme d) (view thyme t))
        (\ (UTCTime d t) -> T.UTCTime (review thyme d) (review thyme t))

instance Thyme T.UTCTime UTCTime where
    {-# INLINE thyme #-}
    thyme = thyme . from utcTime

instance Thyme T.NominalDiffTime NominalDiffTime where
    {-# INLINE thyme #-}
    thyme = iso (microsecondsToNominalDiffTime . round . (*) 1000000)
        (\ (NominalDiffTime t) -> fromRational $ t ^/^ basisValue ())

instance Thyme T.AbsoluteTime AbsoluteTime where
    {-# INLINE thyme #-}
    thyme = iso (`T.diffAbsoluteTime` T.taiEpoch)
            (`T.addAbsoluteTime` T.taiEpoch)
        . thyme . iso (taiEpoch .+^) (.-. taiEpoch)

instance Thyme T.TimeZone TimeZone where
    {-# INLINE thyme #-}
    thyme = id

instance Thyme T.TimeOfDay TimeOfDay where
    {-# INLINE thyme #-}
    thyme = iso ( \ (T.TimeOfDay h m s) -> TimeOfDay h m
            . microsecondsToDiffTime . round $ s * 1000000 )
        ( \ (TimeOfDay h m s) -> T.TimeOfDay h m
            . fromRational $ s ^/^ basisValue () )

instance Thyme T.LocalTime LocalTime where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.LocalTime d t) -> LocalTime (view thyme d) (view thyme t))
        (\ (LocalTime d t) -> T.LocalTime (review thyme d) (review thyme t))

instance Thyme T.ZonedTime ZonedTime where
    {-# INLINE thyme #-}
    thyme = iso
        (\ (T.ZonedTime t z) -> ZonedTime (view thyme t) (view thyme z))
        (\ (ZonedTime t z) -> T.ZonedTime (review thyme t) (review thyme z))

