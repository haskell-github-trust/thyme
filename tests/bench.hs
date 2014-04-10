{-# LANGUAGE CPP #-}

#if HLINT
#include "cabal_macros.h"
#endif

import Prelude
import Control.Arrow
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Criterion
import Criterion.Analysis
import Criterion.Config
import Criterion.Environment
import Criterion.Monad
import Data.Monoid
import Data.Thyme
import Data.Thyme.Calendar.OrdinalDate
import Data.Thyme.Calendar.MonthDay
import Data.Thyme.Time
import qualified Data.Time as T
import qualified Data.Time.Calendar.OrdinalDate as T
import qualified Data.Time.Calendar.WeekDate as T
import qualified Data.Time.Calendar.MonthDay as T
import qualified Data.Time.Clock.POSIX as T
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as QC
#if MIN_VERSION_QuickCheck(2,7,0)
import Test.QuickCheck.Random as QC
#endif
import System.Locale

#if !MIN_VERSION_QuickCheck(2,7,0)
import System.Random
#endif
import Text.Printf

import Common

{-# ANN main "HLint: ignore Use list literal" #-}
main :: IO ()
main = do
    utcs <- unGen (vectorOf samples arbitrary)
#if MIN_VERSION_QuickCheck(2,7,0)
        <$> QC.newQCGen <*> pure 0
#else
        <$> newStdGen <*> pure 0
#endif
    let utcs' = review thyme <$> (utcs :: [UTCTime])
    now <- getCurrentTime
    let now' = thyme # now
    let strs = T.formatTime defaultTimeLocale spec <$> utcs'
    let dt = fromSeconds' 86405
    let dt' = thyme # dt
    let days = utctDay . unUTCTime <$> utcs
    let days' = T.utctDay <$> utcs'
    let years = view (gregorian . _ymdYear) <$> days
    let years' = (\ (y, _m, _d) -> y) . T.toGregorian <$> days'
    let mons = ((isLeapYear . ymdYear) &&& ymdMonth) . view gregorian <$> days
    let ords = ((isLeapYear . odYear) &&& odDay) . view ordinalDate <$> days

    let config = defaultConfig {cfgVerbosity = Last (Just Quiet)}
    (exit . and <=< withConfig config) $ do
        env <- measureEnvironment
        ns <- getConfigItem $ fromLJ cfgResamples
        mapM (benchMean env ns) $

            -- Calendar
            ( "addDays", 1.0
                , nf (addDays 28 <$>) days
                , nf (T.addDays 28 <$>) days' ) :

            ( "toOrdinalDate", 2.7
                , nf (toOrdinalDate <$>) days
                , nf (T.toOrdinalDate <$>) days' ) :

            ( "toGregorian", 4.3
                , nf (toGregorian <$>) days
                , nf (T.toGregorian <$>) days' ) :

            ( "showGregorian", 3.8
                , nf (showGregorian <$>) days
                , nf (T.showGregorian <$>) days' ) :

            ( "toWeekDate", 2.5
                , nf (toWeekDate <$>) days
                , nf (T.toWeekDate <$>) days' ) :

            ( "monthLength", 1.8
                , nf (uncurry monthLength <$>) mons
                , nf (uncurry T.monthLength <$>) mons ) :

            ( "dayOfYearToMonthAndDay", 4.3
                , nf (uncurry dayOfYearToMonthAndDay <$>) ords
                , nf (uncurry T.dayOfYearToMonthAndDay <$>) ords ) :

            ( "isLeapYear", 1.5
                , nf (isLeapYear <$>) years
                , nf (T.isLeapYear <$>) years' ) :

            -- Clock
            ( "addUTCTime", 85
                , nf (addUTCTime dt <$>) utcs
                , nf (T.addUTCTime dt' <$>) utcs' ) :

            ( "diffUTCTime", 22
                , nf (diffUTCTime now <$>) utcs
                , nf (T.diffUTCTime now' <$>) utcs' ) :

            ( "utcTimeToPOSIXSeconds", 10
                , nf (utcTimeToPOSIXSeconds <$>) utcs
                , nf (T.utcTimeToPOSIXSeconds <$>) utcs' ) :

            -- LocalTime
            ( "timeToTimeOfDay", 40
                , nf (timeToTimeOfDay <$>) (utctDayTime . unUTCTime <$> utcs)
                , nf (T.timeToTimeOfDay <$>) (T.utctDayTime <$> utcs') ) :

            ( "utcToLocalTime", 22
                , nf (utcToLocalTime utc <$>) utcs
                , nf (T.utcToLocalTime T.utc <$>) utcs' ) :

            -- Format
            ( "formatTime", 7.5
                , nf (formatTime defaultTimeLocale spec <$>) utcs
                , nf (T.formatTime defaultTimeLocale spec <$>) utcs' ) :

            ( "parseTime", 5.2
                , nf (parse <$>) strs
                , nf (parse' <$>) strs ) :

            []

  where
    samples = 32
    spec = "%F %G %V %u %j %T %s"
    parse = parseTime defaultTimeLocale spec :: String -> Maybe UTCTime
    parse' = T.parseTime defaultTimeLocale spec :: String -> Maybe T.UTCTime

    benchMean env n (name, expected, us, them) = do
        ours <- flip analyseMean n =<< runBenchmark env us
        theirs <- flip analyseMean n =<< runBenchmark env them
        let ratio = theirs / ours
        liftIO . void $ printf
            "%-23s: %6.1fns, %5.1f×; expected %4.1f× : %+3.0f%% %s\n"
            name (ours * 1000000000 / fromIntegral samples)
            ratio expected ((ratio / expected - 1) * 100)
            (if ratio >= expected then "OK." else "oh noes. D:")
        return (ratio >= expected)

