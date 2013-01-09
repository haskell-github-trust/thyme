module Data.Thyme.Clock (
    -- * Universal Time

    -- * Absolute intervals
      DiffTime
    , microsecondsToDiffTime

    -- * UTC
    , UTCTime, UTCView (..)
    , utcTime
    , NominalDiffTime
    , module Data.Thyme.Clock

    -- * Lenses
    , _utctDay, _utctDayTime
    ) where

import Prelude
import Control.Lens
import Data.Thyme.Clock.Scale
import Data.Thyme.Clock.UTC
import Data.Thyme.Clock.POSIX

getCurrentTime :: IO UTCTime
getCurrentTime = fmap (review posixTime) getPOSIXTime

