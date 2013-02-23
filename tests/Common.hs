{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common where

import Prelude
import Control.Applicative
import Control.Lens
import Data.Thyme
import System.Exit
import Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen

exit :: Bool -> IO ()
exit b = exitWith $ if b then ExitSuccess else ExitFailure 1

------------------------------------------------------------------------

instance Arbitrary Day where
    arbitrary = fmap (review gregorian) $ YearMonthDay
        -- FIXME: We disagree with time on how many digits to use for year.
        <$> choose (1000, 9999) <*> choose (1, 12) <*> choose (1, 31)

instance Arbitrary DiffTime where
    arbitrary = fromSeconds <$> (choose (0, 86400.999999) :: Gen Double)

instance Arbitrary UTCTime where
    arbitrary = fmap (review utcTime) $ UTCTime <$> arbitrary <*> arbitrary

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

