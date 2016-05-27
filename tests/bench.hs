module Main where

import Criterion.Main

import Data.Thyme.Clock as TH
import Data.Thyme.Format.Aeson ()
import Data.Time.Clock as TI
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  tinow <- TI.getCurrentTime
  thnow <- TH.getCurrentTime

  let encoded = AE.encode thnow

  defaultMain [
      bgroup "time encode" [ bench "time/encode"   $ nf AE.encode tinow
                           , bench "thyme/encode"  $ nf AE.encode thnow ]
    , bgroup "time decode" [ bench "time/decode"   $ nf (AE.decode :: BL.ByteString -> Maybe TI.UTCTime) encoded
                           , bench "thyme/decode"  $ nf (AE.decode :: BL.ByteString -> Maybe TH.UTCTime) encoded ]
    ]
