{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

#if HLINT
#include "cabal_macros.h"
#endif

import Prelude
import Control.Lens
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import Data.Thyme
import Data.Thyme.Time
import qualified Data.Time as T
import qualified Data.Time.Calendar.OrdinalDate as T
import System.Locale
import Test.QuickCheck

import Common

#if MIN_VERSION_bytestring(0,10,0)
# if MIN_VERSION_bytestring(0,10,2)
import qualified Data.ByteString.Builder as B
# else
import qualified Data.ByteString.Lazy.Builder as B
# endif
import qualified Data.ByteString.Lazy as L
#else
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
#endif

{-# INLINE utf8String #-}
utf8String :: String -> ByteString
#if MIN_VERSION_bytestring(0,10,0)
utf8String = L.toStrict . B.toLazyByteString . B.stringUtf8
#else
utf8String = Text.encodeUtf8 . Text.pack
#endif

------------------------------------------------------------------------

prop_formatTime :: Spec -> RecentTime -> Property
prop_formatTime (Spec spec) (RecentTime t@(review thyme -> t'))
#if MIN_VERSION_QuickCheck(2,7,0)
        = counterexample desc (s == s') where
#else
        = printTestCase desc (s == s') where
#endif
    s = formatTime defaultTimeLocale spec t
    s' = T.formatTime defaultTimeLocale spec t'
    desc = "thyme: " ++ s ++ "\ntime:  " ++ s'

prop_parseTime :: Spec -> RecentTime -> Property
prop_parseTime (Spec spec) (RecentTime orig)
#if MIN_VERSION_QuickCheck(2,7,0)
        = counterexample desc (fmap (review thyme) t == t') where
#else
        = printTestCase desc (fmap (review thyme) t == t') where
#endif
    s = T.formatTime defaultTimeLocale spec (thyme # orig)
    t = parseTime defaultTimeLocale spec s :: Maybe UTCTime
    t' = T.parseTime defaultTimeLocale spec s
    tp = P.parse (timeParser defaultTimeLocale spec) . utf8String
    desc = "input: " ++ show s ++ "\nthyme: " ++ show t
        ++ "\ntime:  " ++ show t' ++ "\nstate: " ++ show (tp s)

prop_ShowRead :: (Eq a, Show a, Read a) => a -> Bool
prop_ShowRead a = (a, "") `elem` reads (show a)

prop_toOrdinalDate :: Int -> Bool
prop_toOrdinalDate day =
  toOrdinalDate (ModifiedJulianDay day) == toOrdinalDateT
  where
    toOrdinalDateT = case T.toOrdinalDate (T.ModifiedJulianDay $ fromIntegral day) of
      (y, yd) -> (fromIntegral y, yd)

------------------------------------------------------------------------

{-# ANN main "HLint: ignore Use list literal" #-}
main :: IO ()
main = (exit . all isSuccess <=< sequence) $
        qc (prop_ShowRead :: Day -> Bool) :
        qc (prop_ShowRead :: DiffTime -> Bool) :
        qc (prop_ShowRead :: NominalDiffTime -> Bool) :
        qc (prop_ShowRead :: UTCTime -> Bool) :
        qcSlow prop_formatTime :
        qcSlow prop_parseTime :
        qc prop_toOrdinalDate :
        []
  where
    isSuccess r = case r of Success {} -> True; _ -> False
    qcArgs = stdArgs { maxSuccess = 10000, maxSize = 100000 }
    qc :: Testable prop => prop -> IO Result
    qc = quickCheckWithResult qcArgs
    qcArgsSlow = stdArgs { maxSuccess = 500, maxSize = 500 }
    qcSlow :: Testable prop => prop -> IO Result
    qcSlow = quickCheckWithResult qcArgsSlow
