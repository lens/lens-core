{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Core.Classes
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCS+FDs
--
----------------------------------------------------------------------------
module Control.Lens.Core.Classes
  (
  -- * Getters
    Gettable(..)
  , noEffect
  -- * Actions
  , Effective(..)
  -- * Setters
  , Settable(..)
  -- * Isomorphisms
  , Isomorphic(..)
  -- * Prisms
  , Prismatic(..)
  -- * Indexable
  , Indexable(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards (Backwards(..))
import Control.Category
import Control.Monad (liftM)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Dual(..))
import Prelude hiding ((.),id)
#ifndef SAFE
import Unsafe.Coerce (unsafeCoerce)
#endif

#ifndef SAFE
#define UNSAFELY(x) unsafeCoerce
#else
#define UNSAFELY(f) (\g -> g `seq` \x -> (f) (g x))
#endif

-------------------------------------------------------------------------------
-- Gettables & Accessors
-------------------------------------------------------------------------------

-- | Generalizing 'Const' so we can apply simple 'Applicative'
-- transformations to it and so we can get nicer error messages
--
-- A 'Gettable' 'Functor' ignores its argument, which it carries solely as a
-- phantom type parameter.
--
-- To ensure this, an instance of 'Gettable' is required to satisfy:
--
-- @'id' = 'fmap' f = 'coerce'@
--
-- Which is equivalent to making a @'Gettable' f@ an \"anyvariant\" functor.
--

class Functor f => Gettable f where
  -- | Replace the phantom type argument.
  coerce :: f a -> f b

instance Gettable (Const r) where
  coerce (Const m) = Const m

instance Gettable f => Gettable (Backwards f) where
  coerce = Backwards . coerce . forwards

instance (Functor f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose . fmap coerce . getCompose

-- | The 'mempty' equivalent for a 'Gettable' 'Applicative' 'Functor'.
noEffect :: (Applicative f, Gettable f) => f a
noEffect = coerce $ pure ()
{-# INLINE noEffect #-}

-------------------------------------------------------------------------------
-- Programming with Effects
-------------------------------------------------------------------------------

-- | An 'Effective' 'Functor' ignores its argument and is isomorphic to a monad wrapped around a value.
--
-- That said, the monad is possibly rather unrelated to any 'Applicative' structure.
class (Monad m, Gettable f) => Effective m r f | f -> m r where
  effective :: m r -> f a
  ineffective :: f a -> m r

instance Effective m r f => Effective m (Dual r) (Backwards f) where
  effective = Backwards . effective . liftM getDual
  ineffective = liftM Dual . ineffective . forwards


-----------------------------------------------------------------------------
-- Settable
-----------------------------------------------------------------------------

-- | Anything 'Settable' must be isomorphic to the 'Identity' 'Functor'.
class Applicative f => Settable f where
  untainted :: f a -> a

  untaintedBy :: (a -> f b) -> a -> b
  untaintedBy g = g `seq` \x -> untainted (g x)

  taintedBy :: (a -> b) -> a -> f b
  taintedBy g = g `seq` \x -> pure (g x)

-- | so you can pass our a 'Control.Lens.Setter.Setter' into combinators from other lens libraries
instance Settable Identity where
  untainted = runIdentity
  {-# INLINE untainted #-}
  untaintedBy = UNSAFELY(runIdentity)
  {-# INLINE untaintedBy #-}
  taintedBy = UNSAFELY(Identity)
  {-# INLINE taintedBy #-}

-- | 'Control.Lens.Fold.backwards'
instance Settable f => Settable (Backwards f) where
  untainted = untainted . forwards
  {-# INLINE untainted #-}

instance (Settable f, Settable g) => Settable (Compose f g) where
  untainted = untainted . untainted . getCompose
  {-# INLINE untainted #-}

-----------------------------------------------------------------------------
-- Isomorphisms
-----------------------------------------------------------------------------

-- | Used to provide overloading of isomorphism application
--
-- An instance of 'Isomorphic' is a 'Category' with a canonical mapping to it from the
-- category of isomorphisms over Haskell types.
class Category k => Isomorphic k where
  -- | Build a simple isomorphism from a pair of inverse functions
  --
  -- @
  -- 'view' ('iso' f g) ≡ f
  -- 'view' ('from' ('iso' f g)) ≡ g
  -- 'set' ('iso' f g) h ≡ g '.' h '.' f
  -- 'set' ('from' ('iso' f g)) h ≡ f '.' h '.' g
  -- @
  iso :: Functor f => (s -> a) -> (b -> t) -> k (a -> f b) (s -> f t)

instance Isomorphic (->) where
  iso sa bt afb s = bt <$> afb (sa s)
  {-# INLINE iso #-}

-----------------------------------------------------------------------------
-- Prisms
-----------------------------------------------------------------------------

-- | Used to provide overloading of prisms.
--
-- An instance of 'Prismatic' is a 'Category' with a canonical mapping to it from the category
-- of embedding-projection pairs over Haskell types.
class Isomorphic k => Prismatic k where
  -- | Build a 'Control.Lens.Prism.Prism'.
  --
  -- @'Either' t a@ is used instead of @'Maybe' a@ to permit the types of @s@ and @t@ to differ.
  prism :: Applicative f => (b -> t) -> (s -> Either t a) -> k (a -> f b) (s -> f t)

instance Prismatic (->) where
  prism bt seta afb = either pure (fmap bt . afb) . seta
  {-# INLINE prism #-}

----------------------------------------------------------------------------
-- Indexed Internals
-----------------------------------------------------------------------------

-- | This class permits overloading of function application for things that
-- also admit a notion of a key or index.
class Indexable i k where
  -- | Build a function from an 'Indexed' function
  indexed :: ((i -> a) -> b) -> k a b

instance Indexable i (->) where
  indexed f = f . const
  {-# INLINE indexed #-}
