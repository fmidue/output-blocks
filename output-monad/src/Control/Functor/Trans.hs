-- |

module Control.Functor.Trans (
  FunctorTrans (..),
  ) where

{-|
The class of functor transformers.
For any functor @f@, the result @t f@ should be a functor,
and 'lift' should be a transformation from @f@ to @t f@,
i.e. it should satisfy the following laws:
--
* @'lift' . 'pure' = 'pure'@
--
* @'lift' (m >> f) = 'lift' m *> ('lift' . f)@
--
-}
class FunctorTrans t where
  -- | Lift a computation from the argument monad to the constructed functor.
  lift :: Functor f => f a -> t f a
