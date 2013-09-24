{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude
#if !MIN_VERSION_base(4,6,0)
    hiding (catch)
#endif
import Control.Exception
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Thyme.Time
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.Exit
import System.FilePath
import System.Posix.Redirect
import System.Random

main :: IO ()
main = do
    defaultMainWithHooksArgs simpleUserHooks
        { buildHook = hook }
        [ "build", "--ghc-option=-ddump-rule-firings" ]
    useless

{-# ANN hook ("HLint: ignore Evaluate" :: String) #-}
{-# ANN hook ("HLint: ignore Use if" :: String) #-}
hook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
hook pd lbi uh bf = do
    -- more reliable way to force a rebuild?
    removeDirectoryRecursive (buildDir lbi </> "rewrite" </> "rewrite-tmp")
        `catch` \ e -> return () `const` (e :: IOException)

    (err, (out, _)) <- redirectStderr . redirectStdout $
        buildHook simpleUserHooks pd lbi uh bf
    let std = T.decodeUtf8 err `T.append` T.decodeUtf8 out

    let fired = foldr ( maybe id (flip (Map.insertWith (+)) (1 :: Int))
            . T.stripPrefix "Rule fired: " ) Map.empty (T.lines std)
    let unmatched = wanted `Map.difference` fired
    case Map.null unmatched of
        True -> mapM_ print (Map.toList $ fired `Map.intersection` wanted)
        False -> do
            putStrLn "Unmatched rules:"
            mapM_ (T.putStrLn . T.append "  ") (Map.keys unmatched)
            exitWith (ExitFailure 1)

useless :: IO ()
useless = do
    print =<< (fmap fromSeconds (randomIO :: IO Float)   :: IO DiffTime)
    print =<< (fmap fromSeconds (randomIO :: IO Double)  :: IO NominalDiffTime)
    print =<< (fmap fromSeconds (randomIO :: IO Int)     :: IO NominalDiffTime)
    print =<< (fmap fromSeconds (randomIO :: IO Int64)   :: IO DiffTime)
    print =<< (fmap fromSeconds (randomIO :: IO Integer) :: IO DiffTime)
    print =<< (fmap realToFrac (randomIO :: IO DiffTime) :: IO NominalDiffTime)
    print =<< (fmap realToFrac (randomIO :: IO NominalDiffTime) :: IO DiffTime)
    print =<< (fmap realToFrac (randomIO :: IO DiffTime) :: IO Double)
    print =<< (fmap realToFrac (randomIO :: IO NominalDiffTime) :: IO Double)
    print =<< (fmap realToFrac (randomIO :: IO Float) :: IO NominalDiffTime)
    print =<< (fmap realToFrac (randomIO :: IO Integer) :: IO DiffTime)

wanted :: Map Text ()
wanted = Map.fromList $ flip (,) () `fmap`
    [ "fromSeconds/Float"
    , "fromSeconds/Double"
    , "fromSeconds/Int"
    , "fromSeconds/Int64"
    , "fromSeconds/Integer"
    , "realToFrac/DiffTime-NominalDiffTime"
    , "realToFrac/NominalDiffTime-DiffTime"
    , "realToFrac/DiffTime-Fractional"
    , "realToFrac/NominalDiffTime-Fractional"
    , "realToFrac/Real-DiffTime"
    , "realToFrac/Real-NominalDiffTime"
    ]

