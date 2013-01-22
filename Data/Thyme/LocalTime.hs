{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Thyme.LocalTime
    ( module Data.Thyme.LocalTime.TimeZone
    , module Data.Thyme.LocalTime.Internal
    , module Data.Thyme.LocalTime
    ) where

import Prelude
import Control.Lens
import Data.Data
import Data.Thyme.Clock
import Data.Thyme.LocalTime.Internal
import Data.Thyme.LocalTime.TimeZone
import Data.Thyme.TH

------------------------------------------------------------------------
-- * Zoned Time

data ZonedTime = ZonedTime
    { zonedTimeToLocalTime :: {-only 4 words…-} {-# UNPACK #-}!LocalTime
    , zonedTimeZone :: !TimeZone
    } deriving (Eq, Ord, Data, Typeable, Show)

{-# INLINE zonedTime #-}
zonedTime :: Simple Iso (TimeZone, UTCTime) ZonedTime
zonedTime = iso toZoned fromZoned where

    {-# INLINE toZoned #-}
    toZoned :: (TimeZone, UTCTime) -> ZonedTime
    toZoned (tz, time) = ZonedTime (view (utcLocalTime tz) time) tz

    {-# INLINE fromZoned #-}
    fromZoned :: ZonedTime -> (TimeZone, UTCTime)
    fromZoned (ZonedTime lt tz) = (tz, review (utcLocalTime tz) lt)

{-# INLINE getZonedTime #-}
getZonedTime :: IO ZonedTime
getZonedTime = utcToLocalZonedTime =<< getCurrentTime

{-# INLINEABLE utcToLocalZonedTime #-}
utcToLocalZonedTime :: UTCTime -> IO ZonedTime
utcToLocalZonedTime time = do
    tz <- getTimeZone time
    return (view zonedTime (tz, time))

------------------------------------------------------------------------
-- * Lenses

thymeLenses ''TimeZone
thymeLenses ''TimeOfDay
thymeLenses ''LocalTime
thymeLenses ''ZonedTime

