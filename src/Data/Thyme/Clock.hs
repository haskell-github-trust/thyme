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
    , getCurrentTime

    -- * Absolute intervals
    , DiffTime

    -- * Time interval conversion
    , TimeDiff (..)
    , toSeconds, fromSeconds
    , toSeconds', fromSeconds'

    -- * Universal Time
    , UniversalTime
    , modJulianDate

    -- * Compatibility
    , getModJulianDate
    , mkModJulianDate
    , secondsToDiffTime
    , picosecondsToDiffTime
    , unUTCTime
    , addUTCTime
    , diffUTCTime
    , toMicroseconds
    , fromMicroseconds

    -- * Lenses
    , _utcvDay, _utcvDayTime
    , _utctDay, _utctDayTime
    ) where

import Prelude
import Control.Lens
import Data.AffineSpace
import Data.Int
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

------------------------------------------------------------------------

-- | Convert a 'UniversalTime' to the fractional number of days since the
-- <http://en.wikipedia.org/wiki/Julian_day#Variants Modified Julian Date epoch>.
--
-- @
-- 'getModJulianDate' = 'view' 'modJulianDate'
-- @
{-# INLINE getModJulianDate #-}
getModJulianDate :: UniversalTime -> Rational
getModJulianDate = view modJulianDate

-- | Construct a 'UniversalTime' from the fractional number of days since the
-- <http://en.wikipedia.org/wiki/Julian_day#Variants Modified Julian Date epoch>.
--
-- @
-- 'mkModJulianDate' = 'review' 'modJulianDate'
-- @
{-# INLINE mkModJulianDate #-}
mkModJulianDate :: Rational -> UniversalTime
mkModJulianDate = review modJulianDate

-- | Construct a 'DiffTime' from some number of seconds.
--
-- This is just 'fromSeconds' with a more constrained type.
--
-- @
-- 'secondsToDiffTime' = 'fromSeconds'
-- @
{-# INLINE secondsToDiffTime #-}
secondsToDiffTime :: Int64 -> DiffTime
secondsToDiffTime = fromSeconds

-- | Construct a 'DiffTime' from some number of picoseconds.
-- The input will be rounded to the nearest microsecond.
--
-- @
-- 'picosecondsToDiffTime' a = 'microseconds' 'Control.Lens.#' 'quot' (a '+' 'signum' a '*' 500000) 1000000
-- @
{-# INLINE picosecondsToDiffTime #-}
picosecondsToDiffTime :: Int64 -> DiffTime
picosecondsToDiffTime a = microseconds # quot (a + signum a * 500000) 1000000

-- | Decompose a 'UTCTime' into a 'UTCView'.
--
-- @
-- 'unUTCTime' = 'view' 'utcTime'
-- @
--
-- For GHC 7.8 or later, there is also the pattern synonym
-- @<Data-Thyme-Clock.html#v:UTCTime UTCTime>@.
{-# INLINE unUTCTime #-}
unUTCTime :: UTCTime -> UTCView
unUTCTime = view utcTime

-- | Add a duration to a point in time.
--
-- @
-- 'addUTCTime' = 'flip' ('.+^')
-- 'addUTCTime' d t ≡ t '.+^' d
-- @
--
-- See also the 'AffineSpace' instance for 'UTCTime'.
{-# INLINE addUTCTime #-}
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime = flip (.+^)

-- | The duration difference between two time points.
--
-- @
-- 'diffUTCTime' = ('.-.')
-- 'diffUTCTime' a b = a '.-.' b
-- @
--
-- See also the 'AffineSpace' instance for 'UTCTime'.
{-# INLINE diffUTCTime #-}
diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime = (.-.)

-- | The number of microseconds in a 'DiffTime' or 'NominalDiffTime'.
--
-- @
-- 'toMicroseconds' :: 'DiffTime' -> 'Int64'
-- 'toMicroseconds' :: 'NominalDiffTime' -> 'Int64'
-- 'toMicroseconds' = 'view' 'microseconds'
-- 'toMicroseconds' d ≡ d '^.' 'microseconds'
-- @
{-# INLINE toMicroseconds #-}
toMicroseconds :: (TimeDiff t) => t -> Int64
toMicroseconds = view microseconds

-- | Construct a 'DiffTime' or 'NominalDiffTime' from a number of
-- microseconds.
--
-- @
-- 'fromMicroseconds' :: 'Int64' -> 'DiffTime'
-- 'fromMicroseconds' :: 'Int64' -> 'NominalDiffTime'
-- 'fromMicroseconds' = 'review' 'microseconds'
-- 'fromMicroseconds' n ≡ 'microseconds' 'Control.Lens.#' n
-- @
{-# INLINE fromMicroseconds #-}
fromMicroseconds :: (TimeDiff t) => Int64 -> t
fromMicroseconds = review microseconds

