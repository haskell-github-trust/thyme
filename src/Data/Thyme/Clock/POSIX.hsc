{-# LANGUAGE ForeignFunctionInterface #-}

#ifndef mingw32_HOST_OS
#include <sys/time.h>
#include <stdlib.h>
#endif

module Data.Thyme.Clock.POSIX
    ( posixDayLength
    , module Data.Thyme.Clock.POSIX
    ) where

import Prelude
import Control.Lens
import Data.AdditiveGroup
import Data.Thyme.Internal.Micro
import Data.Thyme.Clock.Internal
import Data.VectorSpace

#ifdef mingw32_HOST_OS
import System.Win32.Time
#else
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable
#endif

type POSIXTime = NominalDiffTime

{-# INLINE posixTime #-}
posixTime :: Iso' UTCTime POSIXTime
posixTime = iso (\ (UTCRep t) -> t ^-^ unixEpoch)
        (UTCRep . (^+^) unixEpoch) where
    unixEpoch = {-ModifiedJulianDay-}40587 *^ posixDayLength

{-# INLINE getPOSIXTime #-}
getPOSIXTime :: IO POSIXTime
#ifdef mingw32_HOST_OS

getPOSIXTime = do
    FILETIME ft <- System.Win32.Time.getSystemTimeAsFileTime
    return . NominalDiffTime . Micro $
        fromIntegral ft - 116444736000000000{-win32_epoch_adjust-}

#else

getPOSIXTime = allocaBytes #{size struct timeval} $ \ ptv -> do
    memset ptv 0 #{size struct timeval}
    throwErrnoIfMinus1_ "gettimeofday" $ gettimeofday ptv nullPtr
    sec <- #{peek struct timeval, tv_sec} ptv :: IO CLong
    usec <- #{peek struct timeval, tv_usec} ptv :: IO CLong
    return . NominalDiffTime . Micro $
        1000000 * fromIntegral sec + fromIntegral usec

foreign import ccall unsafe "time.h gettimeofday"
    gettimeofday :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "stdlib.h memset"
    memset :: Ptr () -> CInt -> CSize -> IO ()

#endif

