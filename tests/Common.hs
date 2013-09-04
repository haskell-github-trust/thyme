{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common where

import Prelude
import Control.Applicative
import Control.Lens
import Data.AdditiveGroup
import Data.Thyme
import Data.Thyme.Clock.POSIX
import System.Exit
import Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen

exit :: Bool -> IO ()
exit b = exitWith $ if b then ExitSuccess else ExitFailure 1

------------------------------------------------------------------------

-- FIXME: We disagree with time on how many digits to use for year.
newtype RecentTime = RecentTime UTCTime deriving (Show)

instance Arbitrary RecentTime where
    arbitrary = fmap (RecentTime . review utcTime) $ UTCTime
            <$> choose (minDay, maxDay)
            <*> choose (zeroV, pred dayLength) where
        minDay = gregorian # YearMonthDay 1000 1 1
        maxDay = gregorian # YearMonthDay 9999 12 13
        dayLength = posixDayLength ^. from microNominalDiffTime . microDiffTime

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

