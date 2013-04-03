-- | Thyme is a rewrite of the fine @time@ library, with a particular focus
-- on performance for applications that make heavy use of timestamps. For
-- example, 'UTCTime' is represented with μs precision as an
-- 'Data.Int.Int64', which gives a usable range from @-290419-11-07
-- 19:59:05.224192 UTC@ to @294135-11-26 04:00:54.775807 UTC@ in the future.
--
-- Conversions are provided as 'Control.Lens.Iso.Iso''s from the @lens@
-- package, while 'Data.AdditiveGroup.AdditiveGroup',
-- 'Data.VectorSpace.VectorSpace' and 'Data.AffineSpace.AffineSpace' from
-- @vector-space@ allow for more principled calculations instead of 'Num',
-- 'Fractional' & al. Check each module for usage examples, and see
-- <http://hackage.haskell.org/package/lens> or
-- <http://hackage.haskell.org/package/vector-space> for further details.
--
-- Thyme uses strict and unpacked tuples throughout, e.g. 'YearMonthDay' or
-- 'Data.Thyme.Calendar.WeekDate.WeekDate'. Descriptive 'Int' synonyms such
-- as 'Year' and 'DayOfMonth' are also provided.
--
-- On platforms where 'Int' is 64-bits wide, types with an 'Enum' instance
-- can be used as 'Data.IntMap.Key's for 'Data.IntMap.IntMap', preferably
-- via the @EnumMap@ wrapper provided by
-- <http://hackage.haskell.org/package/enummapset-th>. In any case the 'Ord'
-- instances are much faster, if you must use 'Data.Map.Map'.
--
-- "Data.Thyme.Time" is a drop-in compatibility module for exising code.

module Data.Thyme
    ( module Data.Thyme.Calendar
    , module Data.Thyme.Clock
    , module Data.Thyme.Format
    , module Data.Thyme.LocalTime
    ) where

import Data.Thyme.Calendar
import Data.Thyme.Clock
import Data.Thyme.Format
import Data.Thyme.LocalTime

