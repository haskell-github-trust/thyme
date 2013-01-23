{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Criterion
import Criterion.Analysis
import Criterion.Config
import Criterion.Environment
import Criterion.Monad
import Data.Basis
import Data.Monoid
import Data.Thyme
import qualified Data.Time as T
import Data.VectorSpace
import System.Exit
import System.Locale
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
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

spec :: String
spec = "%F %G %V %u %j %T %p %s"

format :: UTCTime -> String
format = formatTime defaultTimeLocale spec
format' :: T.UTCTime -> String
format' = T.formatTime defaultTimeLocale spec

------------------------------------------------------------------------

prop_formatTime :: UTCTime -> Property
prop_formatTime t@(toTime -> t') =
        printTestCase (unlines [s, s']) (s == s') where
    s = format t
    s' = format' t'

------------------------------------------------------------------------

main :: IO ()
main = do
    correct <- fmap (all isSuccess) . mapM quickCheckResult $
        prop_formatTime :
        []

    t <- unGen (vectorOf 10 arbitrary) <$> newStdGen <*> pure 0
    let t' = toTime <$> t
    let s = T.formatTime defaultTimeLocale spec <$> t'
    fast <- fmap and . withConfig config $ do
        env <- measureEnvironment
        ns <- getConfigItem $ fromLJ cfgResamples
        mapM (benchMean env ns) $
            ("formatTime", nf (fmap format) t, nf (fmap format') t', 9) :
            []

    exitWith $ if correct && fast then ExitSuccess else ExitFailure 1
  where
    isSuccess r = case r of Success {} -> True; _ -> False
    config = defaultConfig {cfgVerbosity = Last (Just Quiet)}
    benchMean env n (name, us, them, expected) = do
        ours <- flip analyseMean n =<< runBenchmark env us
        theirs <- flip analyseMean n =<< runBenchmark env them
        let ratio = theirs / ours
        liftIO . void $ printf
            "%s: %.1f× faster; expected %.1f×.\n" name ratio expected
        return (ratio >= expected)

