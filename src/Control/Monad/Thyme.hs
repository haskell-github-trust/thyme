{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Thyme
  ( MonadTime(..)
  ) where

import Control.Monad.Trans
import Data.Thyme

-- | Class of monads which carry the notion of the current time.
class Monad m => MonadTime m where
  currentTime :: m UTCTime

-- | Base instance for IO.
instance {-# OVERLAPPING #-} MonadTime IO where
  currentTime = getCurrentTime

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-} (MonadTime m, MonadTrans t, Monad (t m)) => MonadTime (t m) where
  currentTime = lift currentTime
