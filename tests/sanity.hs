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
import Data.Basis
import Data.List
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

instance Arbitrary Day where
    arbitrary = fmap (review gregorian) $ YearMonthDay
        <$> choose (0, 9999) <*> choose (1, 12) <*> choose (1, 31)

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
    arbitrary = fmap (Spec . unwords . map (\ c -> ['%', c]))
            . Gen.listOf1 . Gen.elements . nub $
        {-aggregate/escape-}"crXx%" ++ {-TimeOfDay-}"RTPpHIklMSqQ" ++
        {-YearMonthDay-}"DFYyCBbhmde" ++ {-MonthDay-}"Bbhmde" ++
        {-OrdinalDate-}"YyCj" ++ {-WeekDate-}"GgfVuAaw" ++
        {-Day-}"UW" ++ {-TimeZone-}"zZ" ++ {-UTCTime-}"s"

------------------------------------------------------------------------

prop_formatTime :: Spec -> UTCTime -> Property
prop_formatTime (Spec spec) t@(toTime -> t')
        = printTestCase desc (s == s') where
    s = formatTime defaultTimeLocale spec t
    s' = T.formatTime defaultTimeLocale spec t'
    desc = "thyme: " ++ s ++ "\ntime:  " ++ s'

------------------------------------------------------------------------

main :: IO ()
main = do
    correct <- fmap (all isSuccess) . mapM quickCheckResult $
        prop_formatTime :
        []

    ts <- Gen.unGen (vectorOf 10 arbitrary) <$> newStdGen <*> pure 0
    let ts' = toTime <$> ts
    fast <- fmap and . withConfig config $ do
        env <- measureEnvironment
        ns <- getConfigItem $ fromLJ cfgResamples
        mapM (benchMean env ns) $
            ( "formatTime", 9
                , nf (formatTime defaultTimeLocale spec <$>) ts
                , nf (T.formatTime defaultTimeLocale spec <$>) ts' ) :
            []

    exitWith $ if correct && fast then ExitSuccess else ExitFailure 1
  where
    isSuccess r = case r of Success {} -> True; _ -> False

    spec = "%F %G %V %u %j %T %p %s"
    config = defaultConfig {cfgVerbosity = Last (Just Quiet)}
    benchMean env n (name, expected, us, them) = do
        ours <- flip analyseMean n =<< runBenchmark env us
        theirs <- flip analyseMean n =<< runBenchmark env them
        let ratio = theirs / ours
        liftIO . void $ printf
            "%s: %.1f× faster; expected %.1f×.\n" name ratio expected
        return (ratio >= expected)

