{-# LANGUAGE ConstraintKinds, DeriveGeneric, DeriveTraversable         #-}
{-# LANGUAGE DerivingStrategies, FlexibleContexts, FlexibleInstances   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, QuantifiedConstraints, PolyKinds   #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TypeApplications #-}
{-# LANGUAGE UndecidableInstances, DerivingVia, TypeOperators          #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Foldable.Monoid where
import Data.Proxy
import Data.Vector (Vector)
import Data.Sequence (Seq)
import Data.Monoid
import Data.Coerce
import Data.Foldable
import Data.Kind
import Data.Traversable
import GHC.Generics

newtype FoldableMonoid f a = FoldableMonoid { runFoldableMonoid :: f a }
  deriving stock (Read, Show, Eq, Ord)

class Functor f => Pointed f where
  iota :: a -> f a
  -- Invariant: fmap f (iota a) == iota (f a)

deriving newtype instance Functor f => Functor (FoldableMonoid f)
deriving newtype instance Foldable f => Foldable (FoldableMonoid f)
deriving newtype instance Pointed f => Pointed (FoldableMonoid f)

class (Pointed f, forall a. c (f a)) => FreeConstruction c f where
  liftMap :: c d => proxy c -> (a -> d) -> f a -> d

castFM :: forall f g a.
          (Pointed f, Foldable f, forall a. Monoid (f a),
           Pointed g, Foldable g, forall a. Monoid (g a)
          ) => f a -> g a
castFM = foldMap iota

instance (Pointed f, Foldable f, forall a. Monoid (f a)) => FreeConstruction Monoid f where
  liftMap _ = foldMap

isoFree :: forall c f g a proxy. (FreeConstruction c f, FreeConstruction c g) => proxy c -> f a -> g a
isoFree pxy = liftMap pxy iota

newtype App f a = App { runApp :: f a }
  deriving (Functor, Applicative, Monad)

instance (Applicative f) => Pointed (App f) where
  iota = App . pure

deriving via (App []) instance Pointed []
deriving via (App Seq) instance Pointed Seq
deriving via (App Vector) instance Pointed Vector

newtype Freeness c f g a = Freeness { runFree :: f a }
  deriving newtype (Functor, Pointed, Applicative, Monad)

toFree :: forall c f g a. (FreeConstruction c f, FreeConstruction c g)
       => g a -> Freeness c f g a
toFree = Freeness . isoFree @c Proxy

fromFree :: forall c f g a. (FreeConstruction c f, FreeConstruction c g)
         => Freeness c f g a -> g a
fromFree = isoFree @c Proxy . runFree

class    (Pointed f, Foldable f, forall a. Monoid (f a)) => FreeMonoid f
instance (Pointed f, Foldable f, forall a. Monoid (f a)) => FreeMonoid f

folding :: (FreeMonoid f) => (a -> b -> b) -> b -> f a -> b
folding g n xs = appEndo (foldMap (Endo . g) xs) n

traverseF :: (FreeMonoid f, Applicative t) => (a -> t b) -> f a -> t (f b)
traverseF f = folding (\a tb -> (<>) <$> (iota <$> f a) <*> tb) (pure mempty)
