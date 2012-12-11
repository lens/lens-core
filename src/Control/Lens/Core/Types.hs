{-# LANGUAGE Rank2Types #-}
module Control.Lens.Core.Types
  (
  -- * Lenses
    Lens
  , IndexedLens
  , SimpleLens
  , SimpleIndexedLens

  -- * Traversals
  , Traversal
  , IndexedTraversal
  , SimpleTraversal
  , SimpleIndexedTraversal

  -- * Prisms
  , Prism
  , SimplePrism

  -- * Isos
  , Iso
  , SimpleIso

  -- * Getters
  , Getter
  , IndexedGetter

  -- * Folds
  , Fold
  , IndexedFold

  -- * Setters
  , Setter
  , IndexedSetter
  , SimpleSetter
  , SimpleIndexedSetter

  -- * Consuming Lenses
  , LensLike
  , SimpleLensLike

  , Overloaded
  , SimpleOverloaded
  ) where

import Control.Applicative
import Control.Lens.Core.Classes

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

-- | A 'Lens' is actually a lens family as described in
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- With great power comes great responsibility and a 'Lens' is subject to the
-- three common sense lens laws:
--
-- 1) You get back what you put in:
--
-- @'Control.Lens.Getter.view' l ('Control.Lens.Setter.set' l b a)  ≡ b@
--
-- 2) Putting back what you got doesn't change anything:
--
-- @'Control.Lens.Setter.set' l ('Control.Lens.Getter.view' l a) a  ≡ a@
--
-- 3) Setting twice is the same as setting once:
--
-- @'Control.Lens.Setter.set' l c ('Control.Lens.Setter.set' l b a) ≡ 'Control.Lens.Setter.set' l c a@
--
-- These laws are strong enough that the 4 type parameters of a 'Lens' cannot
-- vary fully independently. For more on how they interact, read the \"Why is
-- it a Lens Family?\" section of
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- Every 'Lens' can be used directly as a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal'.
--
-- You can also use a 'Lens' for 'Control.Lens.Getter.Getting' as if it were a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Getter.Getter'.
--
-- Since every lens is a valid 'Control.Lens.Traversal.Traversal', the
-- traversal laws are required of any lenses you create:
--
-- @
-- l 'pure' ≡ 'pure'
-- 'fmap' (l f) '.' l g ≡ 'Data.Functor.Compose.getCompose' '.' l ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- @type 'Lens' s t a b = forall f. 'Functor' f => 'LensLike' f s t a b@
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | @type 'SimpleLens' = 'Simple' 'Lens'@
type SimpleLens s a = Lens s s a a

-- | Every 'IndexedLens' is a valid 'Lens' and a valid 'Control.Lens.IndexedTraversal.IndexedTraversal'.
type IndexedLens i s t a b = forall f k. (Indexable i k, Functor f) => k (a -> f b) (s -> f t)

-- | @type 'SimpleIndexedLens' i = 'Simple' ('IndexedLens' i)@
type SimpleIndexedLens i s a = IndexedLens i s s a a

-- |
-- Many combinators that accept a 'Lens' can also accept a
-- 'Control.Lens.Traversal.Traversal' in limited situations.
--
-- They do so by specializing the type of 'Functor' that they require of the
-- caller.
--
-- If a function accepts a @'LensLike' f s t a b@ for some 'Functor' @f@,
-- then they may be passed a 'Lens'.
--
-- Further, if @f@ is an 'Applicative', they may also be passed a
-- 'Control.Lens.Traversal.Traversal'.
type LensLike f s t a b = (a -> f b) -> s -> f t

-- | @type 'SimpleLensLike' f = 'Simple' ('LensLike' f)@
type SimpleLensLike f s a = LensLike f s s a a

-------------------------------------------------------------------------------
-- Overloading function application
-------------------------------------------------------------------------------

-- | @type 'LensLike' f s t a b = 'Overloaded' (->) f s t a b@
type Overloaded k f s t a b = k (a -> f b) (s -> f t)

-- | @type 'SimpleOverloaded' k f s a = 'Simple' ('Overloaded' k f) s a@
type SimpleOverloaded k f s a = Overloaded k f s s a a

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | A 'Traversal' can be used directly as a 'Control.Lens.Setter.Setter' or a 'Fold' (but not as a 'Lens') and provides
-- the ability to both read and update multiple fields, subject to some relatively weak 'Traversal' laws.
--
-- These have also been known as multilenses, but they have the signature and spirit of
--
-- @'traverse' :: 'Traversable' f => 'Traversal' (f a) (f b) a b@
--
-- and the more evocative name suggests their application.
--
-- Most of the time the 'Traversal' you will want to use is just 'traverse', but you can also pass any
-- 'Lens' or 'Control.Lens.Iso.Iso' as a 'Traversal', and composition of a 'Traversal' (or 'Lens' or 'Control.Lens.Iso.Iso') with a 'Traversal' (or 'Lens' or 'Control.Lens.Iso.Iso')
-- using (.) forms a valid 'Traversal'.
--
-- The laws for a Traversal @t@ follow from the laws for Traversable as stated in \"The Essence of the Iterator Pattern\".
--
-- @
-- t 'pure' ≡ 'pure'
-- 'fmap' (t f) '.' t g ≡ 'Data.Functor.Compose.getCompose' '.' t ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- One consequence of this requirement is that a 'Traversal' needs to leave the same number of elements as a
-- candidate for subsequent 'Traversal' that it started with. Another testament to the strength of these laws
-- is that the caveat expressed in section 5.5 of the \"Essence of the Iterator Pattern\" about exotic
-- 'Traversable' instances that 'traverse' the same entry multiple times was actually already ruled out by the
-- second law in that same paper!
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | @type SimpleTraversal = 'Simple' 'Traversal'@
type SimpleTraversal s a = Traversal s s a a

-- | Every indexed traversal is a valid 'Control.Lens.Traversal.Traversal' or
-- 'Control.Lens.IndexedFold.IndexedFold'.
--
-- The 'Indexed' constraint is used to allow an 'IndexedTraversal' to be used
-- directly as a 'Control.Lens.Traversal.Traversal'.
--
-- The 'Control.Lens.Traversal.Traversal' laws are still required to hold.
type IndexedTraversal i s t a b = forall f k. (Indexable i k, Applicative f) => k (a -> f b) (s -> f t)

-- | @type 'SimpleIndexedTraversal' i = 'Simple' ('IndexedTraversal' i)@
type SimpleIndexedTraversal i s a = IndexedTraversal i s s a a

------------------------------------------------------------------------------
-- Setters
------------------------------------------------------------------------------

-- |
-- The only 'Control.Lens.Type.Lens'-like law that can apply to a 'Setter' @l@ is that
--
-- @'set' l y ('set' l x a) ≡ 'set' l y a@
--
-- You can't 'view' a 'Setter' in general, so the other two laws are irrelevant.
--
-- However, two 'Functor' laws apply to a 'Setter':
--
-- @
-- 'over' l 'id' ≡ 'id'
-- 'over' l f '.' 'over' l g ≡ 'over' l (f '.' g)
-- @
--
-- These an be stated more directly:
--
-- @
-- l 'pure' ≡ 'pure'
-- l f . 'untainted' . l g ≡ l (f . 'untainted' . g)
-- @
--
-- You can compose a 'Setter' with a 'Control.Lens.Type.Lens' or a 'Control.Lens.Traversal.Traversal' using ('.') from the Prelude
-- and the result is always only a 'Setter' and nothing more.
--
-- >>> over traverse f [a,b,c,d]
-- [f a,f b,f c,f d]
--
-- >>> over _1 f (a,b)
-- (f a,b)
--
-- >>> over (traverse._1) f [(a,b),(c,d)]
-- [(f a,b),(f c,d)]
--
-- >>> over both f (a,b)
-- (f a,f b)
--
-- >>> over (traverse.both) f [(a,b),(c,d)]
-- [(f a,f b),(f c,f d)]
type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

-- | Every 'IndexedSetter' is a valid 'Setter'
--
-- The 'Control.Lens.Setter.Setter' laws are still required to hold.
type IndexedSetter i s t a b = forall f k.
  (Indexable i k, Settable f) => k (a -> f b) (s -> f t)

-- |
-- @type 'SimpleIndexedSetter' i = 'Simple' ('IndexedSetter' i)@
type SimpleIndexedSetter i s a = IndexedSetter i s s a a

-- |
--
-- A Simple Setter is just a 'Setter' that doesn't change the types.
--
-- These are particularly common when talking about monomorphic containers. /e.g./
--
-- @'sets' Data.Text.map :: 'SimpleSetter' 'Data.Text.Internal.Text' 'Char'@
--
-- @type 'SimpleSetter' = 'Control.Lens.Type.Simple' 'Setter'@
type SimpleSetter s a = Setter s s a a

--------------------------
-- Folds
--------------------------

-- | A 'Fold' describes how to retrieve multiple values in a way that can be composed
-- with other lens-like constructions.
--
-- A @'Fold' s a@ provides a structure with operations very similar to those of the 'Foldable'
-- typeclass, see 'foldMapOf' and the other 'Fold' combinators.
--
-- By convention, if there exists a 'foo' method that expects a @'Foldable' (f a)@, then there should be a
-- @fooOf@ method that takes a @'Fold' s a@ and a value of type @s@.
--
-- A 'Getter' is a legal 'Fold' that just ignores the supplied 'Monoid'
--
-- Unlike a 'Control.Lens.Traversal.Traversal' a 'Fold' is read-only. Since a 'Fold' cannot be used to write back
-- there are no lens laws that apply.
type Fold s a = forall f. (Gettable f, Applicative f) => (a -> f a) -> s -> f s

-- | Every 'IndexedFold' is a valid 'Control.Lens.Fold.Fold'.
type IndexedFold i s a = forall k f.
  (Indexable i k, Applicative f, Gettable f) => k (a -> f a) (s -> f s)

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | A 'Getter' describes how to retrieve a single value in a way that can be
-- composed with other lens-like constructions.
--
-- Unlike a 'Control.Lens.Type.Lens' a 'Getter' is read-only. Since a 'Getter'
-- cannot be used to write back there are no lens laws that can be applied to
-- it. In fact, it is isomorphic to an arbitrary function from @(a -> s)@.
--
-- Moreover, a 'Getter' can be used directly as a 'Control.Lens.Fold.Fold',
-- since it just ignores the 'Applicative'.
type Getter s a = forall f. Gettable f => (a -> f a) -> s -> f s

-- | Every 'IndexedGetter' is a valid 'Control.Lens.IndexedFold.IndexedFold' and 'Getter'.
type IndexedGetter i s a = forall k f. (Indexable i k, Gettable f) => k (a -> f a) (s -> f s)

------------------------------------------------------------------------------
-- Prisms
------------------------------------------------------------------------------

-- | A 'Prism' @l@ is a 0-or-1 target 'Traversal' that can also be turned around with 'remit' to
-- obtain a 'Getter' in the opposite direction.
--
-- There are two laws that a 'Prism' should satisfy:
--
-- First, if I 'remit' or 'review' a value with a 'Prism' and then 'preview' or use ('^?'), I will get it back:
--
-- * @'preview' l ('review' l b) ≡ 'Just' b@
--
-- Second, if you can extract a value @a@ using a Prism @l@ from a value @s@, then the value @s@ is completely described my @l@ and @a@:
--
-- * If @'preview' l s ≡ 'Just' a@ then @'review' l a ≡ s@
--
-- These two laws imply that the 'Traversal' laws hold for every 'Prism' and that we 'traverse' at most 1 element:
--
-- @'Control.Lens.Fold.lengthOf' l x '<=' 1@
--
-- It may help to think of this as a 'Control.Lens.Iso.Iso' that can be partial in one direction.
--
-- Every 'Prism' is a valid 'Traversal'.
--
-- Every 'Control.Lens.Iso.Iso' is a valid 'Prism'.
--
-- For example, you might have a @'Simple' 'Prism' 'Integer' Natural@ allows you to always
-- go from a 'Natural' to an 'Integer', and provide you with tools to check if an 'Integer' is
-- a 'Natural' and/or to edit one if it is.
--
--
-- @
-- 'nat' :: 'Simple' 'Prism' 'Integer' 'Numeric.Natural.Natural'
-- 'nat' = 'prism' 'toInteger' '$' \\ i ->
--    if i '<' 0
--    then 'Left' i
--    else 'Right' ('fromInteger' i)
-- @
--
-- Now we can ask if an 'Integer' is a 'Natural'.
--
-- >>> 5^?nat
-- Just 5
--
-- >>> (-5)^?nat
-- Nothing
--
-- We can update the ones that are:
--
-- >>> (-3,4) & both.nat *~ 2
-- (-3,8)
--
-- And we can then convert from a 'Natural' to an 'Integer'.
--
-- >>> 5 ^. remit nat -- :: Natural
-- 5
--
-- Similarly we can use a 'Prism' to 'traverse' the left half of an 'Either':
--
-- >>> Left "hello" & _left %~ length
-- Left 5
--
-- or to construct an 'Either':
--
-- >>> 5^.remit _left
-- Left 5
--
-- such that if you query it with the 'Prism', you will get your original input back.
--
-- >>> 5^.remit _left ^? _left
-- Just 5
--
-- Another interesting way to think of a 'Prism' is as the categorical dual of a 'Lens'
-- -- a /co/-'Lens', so to speak. This is what permits the construction of 'outside'.
type Prism s t a b = forall k f. (Prismatic k, Applicative f) => k (a -> f b) (s -> f t)

-- | A @'Simple' 'Prism'@.
type SimplePrism s a = Prism s s a a

-----------------------------------------------------------------------------
-- Isos
-----------------------------------------------------------------------------

-- | Isomorphism families can be composed with other lenses using ('.') and 'id'.
type Iso s t a b = forall k f. (Isomorphic k, Functor f) => k (a -> f b) (s -> f t)

-- |
-- @type 'SimpleIso' = 'Control.Lens.Type.Simple' 'Iso'@
type SimpleIso s a = Iso s s a a

