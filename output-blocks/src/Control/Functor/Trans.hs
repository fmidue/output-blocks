{-# LANGUAGE QuantifiedConstraints #-}
-- | Provides the functor transformer class.

module Control.Functor.Trans (
  FunctorTrans (..),
  ) where

{-|
The class of functor transformers.

Lifting a functor to the stacked functor.
-}
class (forall g. Functor g => Functor (t g)) => FunctorTrans t where
  -- | Lift a computation from the argument functor to the constructed functor.
  lift :: Functor f => f a -> t f a
