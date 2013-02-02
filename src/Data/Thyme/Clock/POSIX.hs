module Data.Thyme.Clock.POSIX
    ( posixDayLength
    , module Data.Thyme.Clock.POSIX
    ) where

import Prelude
import Control.Lens
import Data.AdditiveGroup
import Data.Micro
import Data.Thyme.Clock.Internal
import qualified Data.Time.Clock.POSIX as T
import Data.VectorSpace

type POSIXTime = NominalDiffTime

{-# INLINE posixTime #-}
posixTime :: Simple Iso UTCTime POSIXTime
posixTime = iso (\ (UTCRep t) -> t ^-^ unixEpoch)
        (UTCRep . (^+^) unixEpoch) where
    unixEpoch = {-ModifiedJulianDay-}40587 *^ posixDayLength

-- TODO: reimplement without 'T.getPOSIXTime' to avoid 'Integer'?
{-# INLINE getPOSIXTime #-}
getPOSIXTime :: IO POSIXTime
getPOSIXTime = fmap (NominalDiffTime . toMicro . toRational) T.getPOSIXTime

