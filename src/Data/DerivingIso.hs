{-# LANGUAGE ConstraintKinds, DerivingStrategies, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications                 #-}
module Data.DerivingIso where
import Data.Coerce
import Data.Function
import GHC.Generics

newtype SameRepAs a b = SameRepAs { runSameRepAs :: a }
  deriving newtype (Generic)

type Iso a b = (Generic a, Generic b,
                Coercible (Rep a ()) (Rep b ()))

instance (Semigroup b, Iso a b)
      => Semigroup (SameRepAs a b) where
  (<>) = (toA .) . (<>) `on` toB
    where
      toA :: b -> SameRepAs a b
      toA = to @_ @ () . coerce . from @_ @()

      toB :: SameRepAs a b -> b
      toB = to @b @() . coerce . from @_ @()

instance (Monoid b, Iso a b)
       => Monoid (SameRepAs a b) where
  mempty = toA mempty
    where
      toA :: b -> SameRepAs a b
      toA = to @_ @() . coerce . from @_ @ ()
