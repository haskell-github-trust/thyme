{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif

{-| Types and functions for
<http://en.wikipedia.org/wiki/Coordinated_Universal_Time UTC> and
<http://en.wikipedia.org/wiki/Universal_Time#Versions UT1>.

If you don't care about leap seconds, keep to 'UTCTime' and
'NominalDiffTime' for your clock calculations, and you'll be fine.

"Data.Thyme.Time" provides 'Num', 'Real', 'Fractional' and 'RealFrac'
instances for 'DiffTime' and 'NominalDiffTime', but their use is
discouraged. See "Data.Thyme.Docs#spaces" for details.

Use 'fromSeconds' and 'toSeconds' to convert between 'DiffTime'
/ 'NominalDiffTime' and other numeric types; use 'fromSeconds'' for
literals to avoid type defaulting warnings.

-}

module Data.Thyme.Clock (
    -- * UTC
      UTCTime
#if __GLASGOW_HASKELL__ >= 708
    , pattern UTCTime
#endif
    , mkUTCTime
    , utctDay, utctDayTime
    , UTCView (..)
    , utcTime
    , NominalDiffTime
    , module Data.Thyme.Clock

    -- * Absolute intervals
    , DiffTime

    -- * Time interval conversion
    , TimeDiff (..)
    , toSeconds, fromSeconds
    , toSeconds', fromSeconds'

    -- * Universal Time
    , UniversalTime
    , modJulianDate

    -- * Lenses
    , _utcvDay, _utcvDayTime
    , _utctDay, _utctDayTime
    ) where

import Prelude
import Control.Lens
import Data.Thyme.Clock.Internal
import Data.Thyme.Clock.POSIX

-- | Get the current UTC date and time from the local system clock.
--
-- @
-- > 'Data.Thyme.Clock.getCurrentTime'
-- 2016-01-15 13:42:02.287688 UTC
-- @
--
-- See also: 'Data.Thyme.LocalTime.getZonedTime', 'getPOSIXTime'.
getCurrentTime :: IO UTCTime
getCurrentTime = fmap (review posixTime) getPOSIXTime

