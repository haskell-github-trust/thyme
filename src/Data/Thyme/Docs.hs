{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Thyme.Docs
    (
    -- * #api#API
    -- $api

    -- ** #compat#Compatibility with @time@
    -- $compat

    -- ** #spaces#VectorSpace and AffineSpace
    -- $spaces

    -- ** #lenses#Isomorphisms and Lenses
    -- $lenses

    -- * #tutorial#Tutorial
    -- $tutorial

    -- * #impl#Implementation
    -- $impl

    -- ** #range#Range
    -- $range

    -- ** #prec#Precision
    -- $prec

    -- ** #perf#Performance
    -- $perf
    ) where

import Control.Lens
import Data.AffineSpace
import Data.Int
import Data.IntMap (Key, IntMap)
import Data.Map.Strict (Map)
import Data.Thyme
import Data.Thyme.Calendar.WeekDate
import Data.Thyme.Clock.TAI
import Data.Thyme.Format.Human
import Data.Thyme.Time.Core
import qualified Data.Time as T
import Data.VectorSpace

-- Note: {{{ and }}} below are fold markers for Vim. Please ignore.

{- {{{ api -}
{- $api

In general stick to 'UTCTime' and 'NominalDiffTime' for time calculations,
converting from/to 'LocalTime', or 'ZonedTime' et cetera only for
input/output.

Synonyms for 'Int'—such as 'Year', 'Month', and 'DayOfMonth'—are provided
for more descriptive type signatures. There are records (with strict and
unpacked fields) for some common tuples, e.g. 'YearMonthDay', instead of
@('Integer', 'Int', 'Int')@.

On platforms where the native 'Int' is 64-bits wide, types with an 'Enum'
instance such as 'Day' can be used as 'Key's for 'IntMap', preferably via an
@<http://hackage.haskell.org/package/enummapset-th EnumMap>@ wrapper. In any
case the 'Ord' instances are much faster, if you must use 'Map'.

-}{- }}} -}

{- {{{ compat -}
{- $compat

@thyme@ tries to maintain API compatibility with @time@ and each module
exports the same functions as corresponding one in @time@. There are however
some differences to keep in mind:

* 'UTCTime' is an opaque type and there is no @UTCTime@ constructor. Since
  GHC 7.8 you can use the @<Data-Thyme-Clock.html#v:UTCTime UTCTime>@
  pattern synonym. In any case, 'utcTime' can convert it to a 'UTCView'.

* 'UniversalTime' similarly has the
  @<Data-Thyme-Clock.html#v:ModJulianDate ModJulianDate>@ pattern synonym,
  along with 'modJulianDate'.

* 'Year's are 'Int's, not 'Integer's: you may need 'fromIntegral'.

* If you'd rather not use @lens@ or @vector-space@ directly, most modules
  list functions under a ‘Compatibility’ section that mimics the @time@ API.

* Where a third party library uses @time@, you can use 'toThyme' and
  'fromThyme' from "Data.Thyme.Time.Core" to convert between the
  corresponding types.

* "Data.Thyme.Time" provides 'Num', 'Real', 'Fractional', and 'RealFrac'
  instances for 'DiffTime' and 'NominalDiffTime'. Avoid using this in new
  code, <#spaces because of reasons>.

Any other differences not covered here may be unintentional. Please check
<https://github.com/liyang/thyme/issues?q= the list of issues> on GitHub and
file a new one if necessary.

-}{- }}} -}

{- {{{ spaces -}
{- $spaces

One of the issues arising from 'Num', 'Real', 'Fractional', and 'RealFrac'
instances for e.g. 'NominalDiffTime' is that we must also supply nonsensical
operations such as @('*')@: multiplying two quantities of
<https://en.wikipedia.org/wiki/Dimensional_analysis#Definition dimension T>
gives something of dimension /T²/, yet according to the type of @('*')@ the
result is another 'NominalDiffTime' of dimension /T/.

A more principled approach is to invoke our geometric intuition and think of
instants in time as points in a one-dimensional affine space—that is, a line
without an origin per se; a time-line, if you will. Taking the distance
(displacement) between two points in this space amounts to finding the
(signed) duration between two instants in time. These durations are akin to
vectors in the geometric sense, and as such we can add, subtract, or scaled
them by some factor to obtain another duration. Conversely a point plus (or
minus) a vector results in another point, and likewise an instant in time
plus (or minus) a duration takes us to another instant in time.

The 'AffineSpace' and 'VectorSpace' classes from
<http://hackage.haskell.org/package/vector-space vector-space> allows us to
express exactly this idea: 'UTCTime' correspond to points on the time-line,
and is therefore an instance of 'AffineSpace' with the associated

@
type 'Diff' 'UTCTime' = 'NominalDiffTime'
@

as the type of durations between instants. It may help to think of the
provided operations as having these restricted types:

@
('.-.') :: 'UTCTime' -> 'UTCTime' -> 'NominalDiffTime'
('.+^') :: 'UTCTime' -> 'NominalDiffTime' -> 'UTCTime'
('.-^') :: 'UTCTime' -> 'NominalDiffTime' -> 'UTCTime'
@

In turn 'NominalDiffTime' is an instance of 'VectorSpace' (and also
'AdditiveGroup') which comes with the following operations:

@
('^+^') :: 'NominalDiffTime' -> 'NominalDiffTime' -> 'NominalDiffTime'
('^-^') :: 'NominalDiffTime' -> 'NominalDiffTime' -> 'NominalDiffTime'
@

Said instance also defines @type 'Scalar' 'NominalDiffTime' = 'Rational'@,
so we can scale 'NominalDiffTime's thusly:

@
('*^') :: 'Rational' -> 'NominalDiffTime' -> 'NominalDiffTime'
('^*') :: 'NominalDiffTime' -> 'Rational' -> 'NominalDiffTime'
('^/') :: 'NominalDiffTime' -> 'Rational' -> 'NominalDiffTime'
@

On operator naming: remember that @.@ are points, and @^@ are vectors.
Thus @('.+^')@ corresponds to the operation /point plus vector/, and
@('^-^')@ means /vector minus vector/, and so on.

Similarly, 'AbsoluteTime' is an 'AffineSpace', and 'DiffTime' is
a 'VectorSpace', with

@
type 'Diff' 'AbsoluteTime' = 'DiffTime'
type 'Scalar' 'DiffTime' = 'Rational'
@

If you must have 'Num', 'Real', 'Fractional', and 'RealFrac' for 'DiffTime'
and 'NominalDiffTime', they are provided as orhpan instances in the
"Data.Thyme.Time" module.

-}{- }}} -}

{- {{{ lenses -}
{- $lenses

Since a large part of @thyme@'s functionality is involved in converting back
and forth between various representations of time, it's convenient to expose
these as "Control.Lens.Iso"s from the
@<http://hackage.haskell.org/package/lens lens>@ package, and field
accessors as "Control.Lens.Lens"es.

A full @lens@ <https://www.google.com/search?q=haskell+lens+tutorial tutorial>
is however beyond the scope of this documentation. As far as /using/ @thyme@
is concerned, it's sufficient to restrict ourselves to a subset of the
@lens@ API:

@
'view'   :: 'Lens'' s a -> s -> a
'review' :: 'Iso''  s a -> a -> s
'over'   :: 'Lens'' s a -> (a -> a) -> s -> s
@

There are infix versions of the above:

@
('^.') = 'flip' 'view'
('Control.Lens.#')  = 'review'
('%~') = 'over'
@

An 'Iso'' can be used wherever a 'Lens'' is required, so it is helpful to
think of 'view' and 'over' as also having the following types:

@
'view'   :: 'Iso'' s a -> s -> a
'over'   :: 'Iso'' s a -> (a -> a) -> s -> s
@

You can use a @'Lens'' s a@ to extract an @a@ out of an @s@ with 'view'. An
'Iso' s a' means you can also construct an @s@ from an @a@ with 'review'.
Furthermore 'Lens''s and 'Iso''s can be chained with the standard @('.')@
function composition.

@
('.') :: 'Lens'' s u -> 'Lens'' u a -> 'Lens'' s a
('.') :: 'Iso''  s u -> 'Iso''  u a -> 'Iso''  s a
@

Composing an 'Iso'' with a 'Lens'' however ‘downgrades’ the result to
a 'Lens''.

The <#tutorial tutorial below> has some examples.

-}{- }}} -}

{- {{{ tutorial -}
{- $tutorial

Start @ghci@ with the @lens@ and @vector-space@ packages, along with some
preliminary imports:

@
$ ghci -package thyme -package lens -package vector-space
> import "Control.Lens"
> import "Data.AffineSpace"
> import "Data.Thyme"
> import "Data.VectorSpace"
@

Let's begin by getting the current UTC date and time from the local system
clock:

@
> now <- 'getCurrentTime'
> now
2016-04-06 03:50:11.159991 UTC
@

What date and time is it in my local time zone, formatted using the default
locale?

@
> zone <- 'getCurrentTimeZone'
> 'formatTime' 'defaultTimeLocale' \"%c\" $ (zone, now) '^.' 'zonedTime'
"Wed Apr  6 12:50:11 JST 2016"
@

See also: 'getZonedTime'.

What will be the local time-of-day be /1000/ seconds from now?

@
> (now '.+^' 'fromSeconds'' 1000) '^.' 'utcLocalTime' zone '.' '_localTimeOfDay'
12:50:11.159991
@

Approximately how long has it been since the Unix epoch at midnight
/1970-01-01/?

@
> import "Data.Thyme.Format.Human"
> let posixEpoch = 'UTCTime' ('gregorian' 'Control.Lens.#' 'YearMonthDay' 1970 1 1) 'zeroV'
> 'humanTimeDiff' (now '.-.' posixEpoch)
"5 decades"
@

Two months from today, what day of the week will it be?

@
> import "Data.Thyme.Calendar.WeekDate"
> let later = now '&' '_utctDay' '.' 'gregorian' '%~' 'gregorianMonthsClip' 2
> later
2016-06-06 03:50:11.159991 UTC
> later '^.' '_utctDay' '.' 'weekDate'
'WeekDate' {'wdYear' = 2016, 'wdWeek' = 23, 'wdDay' = 1}
> 'formatTime' 'defaultTimeLocale' \"%A\" later
"Monday"
@

-}{- }}} -}

{- {{{ impl -}
{- $impl

The performance improvement of "Data.Thyme" comes chiefly from representing
times internally as a single @newtype@d 'Int64's instead of multiple
'GHC.Integer.GMP.Internals.Integer's, as is the case with
"Data.Time.Clock"@.@'T.UTCTime'. The trade-offs are summarised below.

-}{- }}} -}

{- {{{ range -}
{- $range

'T.UTCTime' from "Data.Time.Clock" has unbounded range, whereas 'UTCTime'
from "Data.Thyme.Clock" has a usable range just under 600,000 years, between
the following times inclusive:

@
-290419-11-07 19:59:05.224192 UTC
 294135-11-26 04:00:54.775807 UTC
@

@thyme@ is therefore not Y294K-compliant.

-}{- }}} -}

{- {{{ prec -}
{- $prec

'UTCTime', 'NominalDiffTime', 'DiffTime', and so on from "Data.Thyme.Clock"
have resolutions of /1 μs/ = /10^-6 s/, while those from "Data.Time.Clock"
have resolutions of /1 ps/ = /10^-12 s/.

However keep in mind that a call to @gettimeofday(3)@ or @clock_gettime(3)@
takes on the order of ~0.4μs on a relatively recent (circa 2016) desktop, so
the coarser resolution is not really an issue in almost all applications.

-}{- }}} -}

{- {{{ perf -}
{- $perf

"Data.Thyme.Clock"@.@'UTCTime' is implemented as @newtype@ wrappers around
a single 'Int64', so that 'addUTCTime' and 'diffUTCTime' both compile down
to a single arithmetic operation. Likewise "Data.Thyme.Clock"@.@'DiffTime'
(and 'NominalDiffTime') are also @newtype@s over 'Int64's.

"Data.Time.Clock"@.@'T.UTCTime' on the other hand comprises of two
'Integer's internally: one for the 'T.Day' and another for the 'T.DiffTime'
time-of-day. Here 'T.diffUTCTime' has to account for the difference in
'T.Day's, while 'T.addUTCTime' must normalise its result to ensure
'T.utctDayTime' stays within 'T.posixDayLength'.

Furthermore, any "Data.Time.Clock"@.@'T.DiffTime' (and 'T.NominalDiffTime')
longer than /~2.15 ms/ exceeds /2^31 ps/, and would overflow a native
'GHC.Prim.Int#' on 32-bit architectures, incurring the cost of using
'GHC.Integer.GMP.Internals.BigNat's inside
'GHC.Integer.GMP.Internals.Integer'. Even for 64-bit 'Int#'s, /2^63 ps/
corresponds to only /~106 days/, and it's not inconceivable to deal with
durations longer than this. In both cases, there is the additional overhead
of working with lifted 'GHC.Integer.GMP.Internals.Integer's.

-}{- }}} -}

