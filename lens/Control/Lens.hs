{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HLINT
#include "cabal_macros.h"
#endif

-- | Small replacement for <http://hackage.haskell.org/package/lens lens>.
module Control.Lens
    ( (&)
    , Iso, Iso', iso
    , from
    , review, ( # )
    , Lens, Lens', lens
    , view, (^.)
    , set, over, (%~), assign, (.=)
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Class as State
import Data.Profunctor
import Data.Profunctor.Unsafe
#if __GLASGOW_HASKELL__ >= 708 && MIN_VERSION_profunctors(4,4,0)
import Data.Coerce
#else
import Unsafe.Coerce
#endif

infixl 1 &
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}

type Overloaded p f s t a b = p a (f b) -> p s (f t)

------------------------------------------------------------------------

type Iso s t a b = forall p f. (Profunctor p, Functor f) => Overloaded p f s t a b
type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

------------------------------------------------------------------------

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}
  lmap f (Exchange sa bt) = Exchange (sa . f) bt
  {-# INLINE lmap #-}
  rmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE rmap #-}
#if __GLASGOW_HASKELL__ >= 708 && MIN_VERSION_profunctors(4,4,0)
  ( #. ) _ = coerce (id :: t -> t) :: forall t u. Coercible t u => u -> t
  ( .# ) p _ = coerce p
#else
  ( #. ) _ = unsafeCoerce
  ( .# ) p _ = unsafeCoerce p
#endif
  {-# INLINE ( #. ) #-}
  {-# INLINE ( .# ) #-}

type AnIso s t a b = Overloaded (Exchange a b) Identity s t a b

from :: AnIso s t a b -> Iso b a t s
from l = case l (Exchange id Identity) of
  Exchange sa bt -> iso (runIdentity #. bt) sa
{-# INLINE from #-}

------------------------------------------------------------------------

newtype Reviewed a b = Reviewed
    { runReviewed :: b
    } deriving (Functor)

instance Profunctor Reviewed where
  dimap _ f (Reviewed c) = Reviewed (f c)
  {-# INLINE dimap #-}
  lmap _ (Reviewed c) = Reviewed c
  {-# INLINE lmap #-}
  rmap = fmap
  {-# INLINE rmap #-}
  Reviewed b .# _ = Reviewed b
  {-# INLINE ( .# ) #-}
#if __GLASGOW_HASKELL__ >= 708 && MIN_VERSION_profunctors(4,4,0)
  ( #. ) _ = coerce (id :: t -> t) :: forall t u. Coercible t u => u -> t
#else
  ( #. ) _ = unsafeCoerce
#endif
  {-# INLINE ( #. ) #-}

type AReview s t a b = Overloaded Reviewed Identity s t a b

review :: AReview s t a b -> b -> t
review p = runIdentity #. runReviewed #. p .# Reviewed .# Identity
{-# INLINE review #-}

infixr 8 #
( # ) :: AReview s t a b -> b -> t
( # ) = review
{-# INLINE ( # ) #-}

------------------------------------------------------------------------

type Lens s t a b = forall f. Functor f => Overloaded (->) f s t a b
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

------------------------------------------------------------------------

type Getting r s a = Overloaded (->) (Const r) s s a a

view :: Getting a s a -> s -> a
view l s = getConst (l Const s)
{-# INLINE view #-}

infixl 8 ^.
(^.) :: s -> Getting a s a -> a
(^.) = flip view
{-# INLINE (^.) #-}

------------------------------------------------------------------------

type Setter s t a b = Overloaded (->) Identity s t a b

set :: Setter s t a b -> b -> s -> t
set l b = runIdentity #. l (\ _ -> Identity b)
{-# INLINE set #-}

over :: Setter s t a b -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)
{-# INLINE over #-}

infixr 4 %~
(%~) :: Setter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

assign :: (MonadState s m) => Setter s s a b -> b -> m ()
assign l b = State.modify (set l b)
{-# INLINE assign #-}

infix 4 .=
(.=) :: (MonadState s m) => Setter s s a b -> b -> m ()
(.=) = assign
{-# INLINE (.=) #-}

