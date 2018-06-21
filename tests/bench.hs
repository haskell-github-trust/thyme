{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Control.DeepSeq (($!!))
import qualified Data.ByteString.Char8 as BS8
import Test.QuickCheck (arbitrary)
import Data.Attoparsec.ByteString (parseOnly)

import qualified Data.Time as Time
import qualified Data.Thyme as Thyme

main :: IO ()
main = do
    utcthyme <- generate arbitrary :: IO Thyme.UTCTime
    let isoFormatString = "%Y-%m-%dT%H:%M:%S%N"
        renderIsoTime = Thyme.formatTime Thyme.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%N"
    string <- return $!! renderIsoTime utcthyme
    bytestring <- return $!! BS8.pack (renderIsoTime utcthyme)

    defaultMain
        [ bgroup "parsing"
            [ bench "Thyme.parseTime" $ nf
                ((Thyme.parseTime Thyme.defaultTimeLocale isoFormatString :: String -> Maybe Thyme.UTCTime))
                string
            , bench "Time.parseTimeM" $ nf
                (Time.parseTimeM True Time.defaultTimeLocale isoFormatString :: String -> Maybe Time.UTCTime)
                string
            , bench "Thyme.timeParser" $ nf
                (fmap (Thyme.buildTime @Thyme.UTCTime) . parseOnly (Thyme.timeParser Thyme.defaultTimeLocale isoFormatString))
                bytestring
            ]
        ]
