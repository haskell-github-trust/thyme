module Data.Thyme.Clock.POSIX
    ( posixDayLength
    , module Data.Thyme.Clock.POSIX
    ) where

import Prelude
import Control.Lens
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Micro
import qualified Data.Time.Clock.POSIX as T
import Data.Thyme.Calendar.Internal
import Data.Thyme.Clock.Scale
import Data.Thyme.Clock.UTC

type POSIXTime = NominalDiffTime

{-# INLINE posixTime #-}
posixTime :: Simple Iso UTCTime POSIXTime
posixTime = iso toPOSIX fromPOSIX where
    unixEpochDay = ModifiedJulianDay 40587

    {-# INLINE toPOSIX #-}
    toPOSIX :: UTCTime -> POSIXTime
    toPOSIX t = t .-. review utcTime (UTCTime unixEpochDay zeroV)

    {-# INLINE fromPOSIX #-}
    fromPOSIX :: POSIXTime -> UTCTime
    fromPOSIX (NominalDiffTime d) = review utcTime $
        UTCTime unixEpochDay (DiffTime d)

-- TODO: reimplement without 'T.getPOSIXTime' to avoid 'realToFrac'?
{-# INLINE getPOSIXTime #-}
getPOSIXTime :: IO POSIXTime
getPOSIXTime = fmap {-realToFrac-}(NominalDiffTime . toMicro . toRational) T.getPOSIXTime

