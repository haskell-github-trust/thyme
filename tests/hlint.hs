module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Exit

main :: IO ()
main = (`unless` exitFailure) . null =<< hlint
    [ "src", "tests"
    , "--cpp-define=HLINT"
    , "--cpp-include=include"
    , "--cpp-define=SHOW_INTERNAL=1"
    , "-XNoUnboxedTuples"
    , "-i", "Unused LANGUAGE pragma"
    , "-i", "Reduce duplication"
    , "-i", "Redundant lambda"
    , "-i", "Use if"
    , "-i", "Use import/export shortcut"
    ]

