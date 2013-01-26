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
import Data.Thyme.Clock
import Data.Thyme.LocalTime.Internal
import Data.Thyme.LocalTime.TimeZone
import Data.Thyme.TH

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

