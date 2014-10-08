module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Exit

main :: IO ()
main = (`unless` exitFailure) . null =<< hlint
    [ "src", "tests"
    , "--cpp-define=HLINT=1"
    , "--cpp-include=include"
    , "--cpp-include=dist/build/autogen"
    , "--cpp-define=SHOW_INTERNAL=1"
    , "-i", "Reduce duplication"
    , "-i", "Redundant lambda"
    , "-i", "Use if"
    , "-i", "Use import/export shortcut"
    ]

