{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Criterion
import Criterion.Analysis
import Criterion.Config
import Criterion.Environment
import Criterion.Monad
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Basis
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Thyme
import qualified Data.Time as T
import Data.VectorSpace
import System.Exit
import System.Locale
import System.Random
import Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen
import Text.Printf

#if MIN_VERSION_bytestring(0,10,0)
# if MIN_VERSION_bytestring(0,10,2)
import qualified Data.ByteString.Builder as B
# else
import qualified Data.ByteString.Lazy.Builder as B
# endif
import qualified Data.ByteString.Lazy as L
#else
import qualified Data.ByteString.UTF8 as U8
#endif

{-# INLINE utf8String #-}
utf8String :: String -> ByteString
#if MIN_VERSION_bytestring(0,10,0)
utf8String = L.toStrict . B.toLazyByteString . B.stringUtf8
#else
utf8String = U8.fromString
#endif

------------------------------------------------------------------------

instance Arbitrary Day where
    arbitrary = fmap (review gregorian) $ YearMonthDay
        -- FIXME: We disagree with time on how many digits to use for year.
        <$> choose (1000, 9999) <*> choose (1, 12) <*> choose (1, 31)

instance Arbitrary DiffTime where
    arbitrary = (^*) (basisValue ()) . toRational <$> (choose (0, 86400.999999) :: Gen Double)

instance Arbitrary UTCTime where
    arbitrary = fmap (review utcTime) $ UTCTime <$> arbitrary <*> arbitrary

toTime :: UTCTime -> T.UTCTime
toTime (view utcTime -> UTCTime (ModifiedJulianDay d) t) = T.UTCTime
    (T.ModifiedJulianDay $ fromIntegral d) (fromRational $ t ^/^ basisValue ())

(^/^) :: (HasBasis v, Basis v ~ (), Scalar v ~ s, Fractional s) => v -> v -> s
x ^/^ y = decompose' x () / decompose' y ()

------------------------------------------------------------------------

newtype Spec = Spec String deriving (Show)

instance Arbitrary Spec where
    arbitrary = do
        -- Pick a non-overlapping day spec generator.
        day <- Gen.elements
            [ spec {-YearMonthDay-}"DFYyCBbhmde"
            , spec {-OrdinalDate-}"YyCj"
            -- TODO: time only consider the presence of %V as
            -- indication that it should parse as WeekDate
            , (++) "%V " <$> spec {-WeekDate-}"GgfuwAa"
            , spec {-SundayWeek-}"YyCUuwAa"
            , spec {-MondayWeek-}"YyCWuwAa"
            ] :: Gen (Gen String)
        -- Pick a non-overlapping day & tod spec generator.
        time <- Gen.frequency
            [ (16, pure $ Gen.frequency
                [ (8, day)
                , (4, rod)
                , (2, h12)
                , (1, sec)
                , (1, spec {-TimeZone-}"zZ")
                ] )
            -- TODO: these are broken due to issues above and below
            -- , (2, pure $ spec {-aggregate-}"crXx")
            , (1, pure $ spec {-UTCTime-}"s")
            ] :: Gen (Gen String)
        fmap (Spec . unwords) . listOf1 $ frequency
            [(16, time), (4, string), (1, pure "%%")]
      where
        spec = Gen.elements . fmap (\ c -> ['%', c])
        string = filter ('%' /=) <$> arbitrary
        -- TODO: time discards %q %Q or %p %P after setting %S or hours
        -- respectively. Fudge it by always including %q and %p at end.
        -- tod = spec {-TimeOfDay-}"RTPpHIklMSqQ"
        rod = spec {-RestOfDay-}"RHkMqQ"
        sec = (++ " %q") <$> spec {-seconds-}"ST"
        h12 = (++ " %p") <$> spec {-12-hour-}"Il"

------------------------------------------------------------------------

prop_formatTime :: Spec -> UTCTime -> Property
prop_formatTime (Spec spec) t@(toTime -> t')
        = printTestCase desc (s == s') where
    s = formatTime defaultTimeLocale spec t
    s' = T.formatTime defaultTimeLocale spec t'
    desc = "thyme: " ++ s ++ "\ntime:  " ++ s'

prop_parseTime :: Spec -> UTCTime -> Property
prop_parseTime (Spec spec) (T.formatTime defaultTimeLocale spec . toTime -> s)
        = printTestCase desc (fmap toTime t == t') where
    t = parseTime defaultTimeLocale spec s
    t' = T.parseTime defaultTimeLocale spec s
    tp = P.parseOnly (timeParser defaultTimeLocale spec) . utf8String
    desc = "input: " ++ show s ++ "\nthyme: " ++ show t
        ++ "\ntime:  " ++ show t' ++ "\nstate: " ++ show (tp s)

------------------------------------------------------------------------

main :: IO ()
main = do
    correct <- fmap (all isSuccess) . mapM quickCheckResult $
        prop_formatTime :
        prop_parseTime :
        []

    ts <- Gen.unGen (vectorOf 10 arbitrary) <$> newStdGen <*> pure 0
    let ts' = toTime <$> ts
    let ss = T.formatTime defaultTimeLocale spec <$> ts'
    fast <- fmap and . withConfig config $ do
        env <- measureEnvironment
        ns <- getConfigItem $ fromLJ cfgResamples
        mapM (benchMean env ns) $
            ( "formatTime", 9
                , nf (formatTime defaultTimeLocale spec <$>) ts
                , nf (T.formatTime defaultTimeLocale spec <$>) ts' ) :
            ( "parseTime", 4.5, nf (parse <$>) ss, nf (parse' <$>) ss ) :
            []

    exitWith $ if correct && fast then ExitSuccess else ExitFailure 1
  where
    isSuccess r = case r of Success {} -> True; _ -> False
    config = defaultConfig {cfgVerbosity = Last (Just Quiet)}

    spec = "%F %G %V %u %j %T %s"
    parse = parseTime defaultTimeLocale spec :: String -> Maybe UTCTime
    parse' = T.parseTime defaultTimeLocale spec :: String -> Maybe T.UTCTime

    benchMean env n (name, expected, us, them) = do
        ours <- flip analyseMean n =<< runBenchmark env us
        theirs <- flip analyseMean n =<< runBenchmark env them
        let ratio = theirs / ours
        liftIO . void $ printf
            "%s: %.1f× faster; expected %.1f×.\n" name ratio expected
        return (ratio >= expected)

