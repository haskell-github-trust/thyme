{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.Maybe (fromMaybe)
import Control.DeepSeq (($!!))
import qualified Data.ByteString.Char8 as BS8
import Test.QuickCheck (arbitrary, generate)
import Data.Attoparsec.ByteString (parseOnly)

import qualified Data.Time as Time
import qualified Data.Thyme as Thyme

main :: IO ()
main = do
    utcthyme <- generate arbitrary :: IO Thyme.UTCTime
    let isoFormatString = "%Y-%m-%dT%H:%M:%S"
        renderIsoTime = Thyme.formatTime Thyme.defaultTimeLocale isoFormatString
        thymeParser :: String -> Thyme.UTCTime
        thymeParser =
            fromMaybe (error "Failed in thymeParser")
            . Thyme.parseTime Thyme.defaultTimeLocale isoFormatString
        timeParser :: String -> Time.UTCTime
        timeParser =
            fromMaybe (error "Failed in timeParser")
            . Time.parseTimeM True Time.defaultTimeLocale isoFormatString
        thymeAttoparsec :: BS8.ByteString -> Thyme.UTCTime
        thymeAttoparsec =
            Thyme.buildTime @Thyme.UTCTime
            . either error id
            . parseOnly (Thyme.timeParser Thyme.defaultTimeLocale isoFormatString)

    string <- return $!! renderIsoTime utcthyme
    bytestring <- return $!! BS8.pack (renderIsoTime utcthyme)

    defaultMain
        [ bgroup "parsing"
            [ bench "Time.parseTimeM" $ nf timeParser  string
            , bench "Thyme.parseTime" $ nf thymeParser string
            , bench "Thyme.timeParser" $ nf thymeAttoparsec bytestring
            ]
        ]
