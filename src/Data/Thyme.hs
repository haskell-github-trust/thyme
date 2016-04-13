{- | "Data.Thyme" is a speed-optimized rewrite (with a bit of extra type-safety)
of the excellent "Data.Time" module in the @time@ library.

= Design

== Implementation

The performance improvement of "Data.Thyme" comes chiefly from representing
times internally as primitive unlifted 'Data.Int.Int64' instead of
lifted 'Integer', as is done in "Data.Time".

The tradeoffs for this design can be generalized as:

    * Data.Time.Clock.'Data.Time.Clock.DiffTime' has a precision
      of /1 picosecond/.

        - Data.Thyme.Clock.'Data.Thyme.Clock.DiffTime' has a precision
          of /1 microsecond/.

    * Data.Time.Clock.'Data.Time.Clock.UTCTime' has unbounded range.

        - Data.Thyme.Clock.'Data.Thyme.Clock.UTCTime' has a range of about a
          half million years, from /-290419-11-07 19:59:05.224192 UTC/
          to /294135-11-26 04:00:54.775807 UTC/. It is therefore not
          Y294K-compliant.



"Data.Thyme" uses strict and unpacked tuples throughout, e.g. 'YearMonthDay' or
'Data.Thyme.Calendar.WeekDate.WeekDate'. Descriptive 'Int' synonyms such
as 'Year' and 'DayOfMonth' are also provided.

On platforms where 'Int' is 64-bits wide, types with an 'Enum' instance
such as 'Day' can be used as 'Data.IntMap.Key's for 'Data.IntMap.IntMap', preferably
via the @EnumMap@ wrapper provided by
<http://hackage.haskell.org/package/enummapset-th>. In any case the 'Ord'
instances are much faster, if you must use 'Data.Map.Map'.

== API

Conversions are provided as "Control.Lens.Iso"s from the
<http://hackage.haskell.org/package/lens lens> package, while
'Data.AdditiveGroup.AdditiveGroup', 'Data.VectorSpace.VectorSpace' and
'Data.AffineSpace.AffineSpace' from
<http://hackage.haskell.org/package/vector-space vector-space> allow for
more principled operations instead of 'Num', 'Fractional' & al.

"Data.Thyme.Time" is a drop-in replacement module compatible with
"Data.Time".

= Brief Tutorial By Example

===== Let's start by getting the current UTC date and time from the local system clock.

@
import "Data.Thyme.Clock"

__t₀__ <- 'Data.Thyme.Clock.getCurrentTime'
'show' __t₀__
@
@
"2016-04-06 03:50:11.159991 UTC"
@

===== What date and time is it in my local time zone, formatted to my default locale?

@
import "Control.Lens"
import "System.Locale"
import "Data.Thyme.LocalTime"
import "Data.Thyme.Format"

tz <- 'Data.Thyme.LocalTime.getCurrentTimeZone'
'Data.Thyme.Format.formatTime' 'System.Locale.defaultTimeLocale' \"%c\" $ (tz, __t₀__) 'Control.Lens.Operators.^.' 'Data.Thyme.LocalTime.zonedTime'
@
@
"Wed Apr  6 12:50:11 JST 2016"
@

===== What wall-clock time will it be /1000/ seconds from now?

@
import "Data.AffineSpace"

(tz, __t₀__ 'Data.AffineSpace.+^' 'Data.Thyme.Clock.fromSeconds'' 1000) 'Control.Lens.Operators.^.' 'Data.Thyme.LocalTime.zonedTime' . 'Data.Thyme.LocalTime._zonedTimeToLocalTime' . 'Data.Thyme.LocalTime._localTimeOfDay'
@
@
13:06:51.159991
@

===== About how long has it been since the Unix Era began at the Unix Epoch of /midnight, January first, 1970/?

@
import "Data.Thyme.Format.Human"
import "Data.Thyme.Time.Core"

"We are about " ++ 'Data.Thyme.Format.Human.humanTimeDiff' (__t₀__ 'Data.AffineSpace..-.' 'Data.Thyme.Time.Core.mkUTCTime' ('Data.Thyme.Time.Core.fromGregorian' 1970 1 1) ('Data.Thyme.Clock.hhmmss' 0 0 0)) ++ " into the Unix Era."
@
@
"We are about 5 decades into the Unix Era."
@

===== Two months from today, what day of the week will it be?

@
import "Data.Thyme.Calendar"

'Data.Thyme.Format.formatTime' 'System.Locale.defaultTimeLocale' \"%A\" $ 'Control.Lens.Getter.view' 'Data.Thyme.Calendar.WeekDate.weekDate' $ 'Control.Lens.Setter.over' 'Data.Thyme.Calendar.gregorian' ('Data.Thyme.Calendar.gregorianMonthsClip' 2) $ 'Control.Lens.Getter.view' 'Data.Thyme.Clock._utctDay' __t₀__
@
@
\"Monday\"
@

-}

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
