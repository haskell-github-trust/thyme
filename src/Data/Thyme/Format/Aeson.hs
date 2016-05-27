{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if HLINT
#include "cabal_macros.h"
#endif

-- | Instances of 'FromJSON' and 'ToJSON' for 'UTCTime' and 'ZonedTime',
-- along with a newtype wrapper 'DotNetTime'.
module Data.Thyme.Format.Aeson
    ( DotNetTime (..)
    ) where

import Prelude
import Control.Applicative
import Data.Aeson hiding (DotNetTime (..))
import Data.Aeson.Types hiding (DotNetTime (..))
import Data.Data
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Data.Thyme
import Data.Thyme.Format.DateFast (parseFastUtc)
import Data.Thyme.Format.DateEncode (utcTimeBuilder, quote)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)

-- Copyright:   (c) 2011, 2012, 2013 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.

------------------------------------------------------------------------
-- Copypasta from aeson-0.7.1.0:Data.Aeson.Types.Internal

-- | A newtype wrapper for 'UTCTime' that uses the same non-standard
-- serialization format as Microsoft .NET, whose @System.DateTime@
-- type is by default serialized to JSON as in the following example:
--
-- > /Date(1302547608878)/
--
-- The number represents milliseconds since the Unix epoch.
newtype DotNetTime = DotNetTime {
      fromDotNetTime :: UTCTime
    } deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

------------------------------------------------------------------------
-- Copypasta from aeson-0.7.1.0:Data.Aeson.Types.Instances

instance ToJSON DotNetTime where
    toJSON (DotNetTime t) =
        String (pack (secs ++ formatMillis t ++ ")/"))
      where secs  = formatTime defaultTimeLocale "/Date(%s" t
    {-# INLINE toJSON #-}

instance FromJSON DotNetTime where
    parseJSON = withText "DotNetTime" $ \t ->
        let (s,m) = T.splitAt (T.length t - 5) t
            t'    = T.concat [s,".",m]
        in case parseTime defaultTimeLocale "/Date(%s%Q)/" (unpack t') of
             Just d -> pure (DotNetTime d)
             _      -> fail "could not parse .NET time"
    {-# INLINE parseJSON #-}

instance ToJSON ZonedTime where
    toJSON t = String $ pack $ formatTime defaultTimeLocale format t
      where
        format = "%FT%T." ++ formatMillis t ++ tzFormat
        tzFormat
          | 0 == timeZoneMinutes (zonedTimeZone t) = "Z"
          | otherwise = "%z"

formatMillis :: (FormatTime t) => t -> String
formatMillis t = take 3 $ formatTime defaultTimeLocale "%q" t

instance FromJSON ZonedTime where
    parseJSON (String t) =
      tryFormats alternateFormats
      <|> fail "could not parse ECMA-262 ISO-8601 date"
      where
        tryFormat f =
          case parseTime defaultTimeLocale f (unpack t) of
            Just d -> pure d
            Nothing -> empty
        tryFormats = foldr1 (<|>) . map tryFormat
        alternateFormats =
          dateTimeFmt defaultTimeLocale :
          distributeList ["%Y", "%Y-%m", "%F"]
                         ["T%R", "T%T", "T%T%Q", "T%T%QZ", "T%T%Q%z"]

        distributeList xs ys =
          foldr (\x acc -> acc ++ distribute x ys) [] xs
        distribute x = map (mappend x)

    parseJSON v = typeMismatch "ZonedTime" v

instance ToJSON UTCTime where
    toEncoding t = unsafeToEncoding $ quote (utcTimeBuilder t)
    {-# INLINE toEncoding #-}
    toJSON t = String $ decodeUtf8 $ toStrict $ toLazyByteString (utcTimeBuilder t)
    {-# INLINE toJSON #-}

instance FromJSON UTCTime where
    parseJSON = withText "UTCTime" $ parseFastUtc
    {-# INLINE parseJSON #-}
