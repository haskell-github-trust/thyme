{-# OPTIONS_GHC -O2 -dumpdir dump -ddump-to-file -ddump-rule-firings #-}

import Prelude
import Data.Int
import Data.List (stripPrefix)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Thyme.Time
import System.Exit
import System.Random

main :: IO ()
main = do
    useless
    checkRuleFirings "dump/tests/rewrite.dump-rule-firings"

checkRuleFirings :: FilePath -> IO ()
checkRuleFirings file = do
    dump <- readFile file
    let strip = maybe id Set.insert . stripPrefix "Rule fired: "
    let fired = foldr strip Set.empty (lines dump)
    let unmatched = wanted `Set.difference` fired
    case Set.null unmatched of
        True -> do
            putStrLn "All wanted rules fired."
            exitSuccess
        False -> do
            putStrLn "Unmatched rules:"
            mapM_ (putStrLn . (++) "  ") (Set.toList unmatched)
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

wanted :: Set String
wanted = Set.fromList
    [ "fromSeconds/Float (Data.Thyme.Clock.Internal)"
    , "fromSeconds/Double (Data.Thyme.Clock.Internal)"
    , "fromSeconds/Int (Data.Thyme.Clock.Internal)"
    , "fromSeconds/Int64 (Data.Thyme.Clock.Internal)"
    , "fromSeconds/Integer (Data.Thyme.Clock.Internal)"
    , "realToFrac/DiffTime-NominalDiffTime (Data.Thyme.Time)"
    , "realToFrac/NominalDiffTime-DiffTime (Data.Thyme.Time)"
    , "realToFrac/DiffTime-Fractional (Data.Thyme.Time)"
    , "realToFrac/NominalDiffTime-Fractional (Data.Thyme.Time)"
    , "realToFrac/Real-DiffTime (Data.Thyme.Time)"
    , "realToFrac/Real-NominalDiffTime (Data.Thyme.Time)"
    ]