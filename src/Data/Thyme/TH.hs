{-# OPTIONS_HADDOCK hide #-}

module Data.Thyme.TH (thymeLenses) where

import Prelude
import Control.Lens
import Language.Haskell.TH

thymeLenses :: Name -> Q [Dec]
thymeLenses = makeLensesWith $ lensRules
    & lensField .~ Just . (:) '_'

