{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RankNTypes      #-}

module Control.Interpret where

import           Control.Category
import           Prelude          hiding (id, (.))
import qualified Prelude          as P

-- | A morphism in a category of monad constraints.
-- An interpreter takes a monad instance constrained by @a@,
-- and interprets it under the constraint @b@.
-- The input monad is a rank n type, so callers can't choose what monad to use.
-- Callers can only use operations supplied by the @a@ constraint.
-- The interpreter chooses what concrete monad to use to interpret this,
-- and may only use operations of the @b@ constraint in that interpretation.
--
-- The actual interpretation is a natural transformation.
newtype Interpret a b = MkInterpret
  { interpret :: forall n x. b n => (forall m. a m => m x) -> n x }

instance Category Interpret where
  id = MkInterpret P.id
  MkInterpret f . MkInterpret g = MkInterpret $ \x -> f (g x)

-- | A functor from the 'Interpret' category to Hask.
-- Ideally, this would be a more general class, like this
--
-- > class (Category dom, Category cod) => CategoryFunctor dom cod f where
-- >   cmap :: dom a b -> cod (f a) (f b)
--
-- Instances of 'InterpretFunctor' would be written like this
--
-- > instance CategoryFunctor Interpret (->) (...) where ...
--
-- But that would not be derivable with Generic1.
-- Granted, neither is this (yet).
-- But future versions of Generic1 will be poly-kinded,
-- which will allow this to be derivable.
class InterpretFunctor f where
  imap :: Interpret a b -> f a -> f b

instance InterpretFunctor (Interpret a) where
  imap = (.)
