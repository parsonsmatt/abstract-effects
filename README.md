# interpreters

`mtl`-style monad classes provide concise interfaces for monads.
Often, a set of operations are best isolated in such a class,
even if they can be implemented directly in terms of a specific monad.
This promotes isolation of effects.
As such, it becomes necessary to convert (or *interpret*) one class to another.
Such a conversion is known as a natural transformation.
Natural transformations have the following form.

```haskell
type Nat f g = forall x. f x -> g x
```

That is, given any instance of `f`,
it should create an instance of `g` with the same type argument.
But in terms of `mtl`-style classes, we don't want to transform concrete monads.
We want to transform *classes* of monads.
So given an expression of some class `a`,
we want to create an expression of another class `b`.
This means this kind of natural transformation (henceforth known as interpreter)
needs to be able to internally choose the concrete implementation of `a`
in order to write an expression in `b`.
In practice this uses `RankNTypes` to ensure the argument only uses `a` methods.

```haskell
type Interpret a b = forall n x. b n => (forall m. a m => m x) -> n x
```

It's like a natural transformation for classes instead of monads themselves.
As an example, consider layering a monad for a REST API over an HTTP monad.

```haskell
class Monad m => MonadHttp m where
  httpGet :: String -> m String

class Monad m => MonadRestApi m where
  getUserIds :: m [Int]

-- Free monads are an easy way to use this pattern, but not necessary.
data RestApiCommands a = GetUserIds ([Int] -> a) deriving Functor

instance MonadRestApi (Free RestApiCommands) where
  getUserIds = liftF $ GetUserIds id

-- iterA :: (Functor f, Monad m) => (f (m a) -> m a) -> Free f a -> m a
runRestApi :: Interpret MonadRestApi MonadHttp
runRestApi = iterA go where
  go (GetUserIds f) = do
    response <- httpGet "http://url.com/api/users"
    f (read response)
```

`runRestApi` gets to interpret the argument as
any instance of `MonadRestApi` that it wants,
and it may not choose any instance of `MonadHttp`.
This way, users of `MonadRestApi` can only use the operations in the class,
and the interpreter is similarly restricted to `MonadHttp` operations.

If you define `Interpret` as a `newtype`, it forms an instance of `Category`.

```haskell
import Control.Category
import Prelude hiding (id, (.))
import qualified Prelude as P

newtype Interpret a b = MkInterpret
  { interpret :: forall n x. b n => (forall m. a m => m x) -> n x }

instance Category Interpret where
  id = MkInterpret P.id
  MkInterpret f . MkInterpret g = MkInterpret $ \x -> f (g x)
```

Using this as a category makes it easy to compose interpreters.

```haskell
runRestApi :: Interpret MonadRestApi MonadHttp
runRestApi = ...

runHttp :: Interpret MonadHttp MonadIO
runHttp = ...

runApplication :: Interpret MonadRestApi MonadIO
runApplication = runHttp . runRestApi

-- Useful for testing
mockHttp :: Interpret MonadHttp (MonadState MockData)
mockHttp = ...

mockApplication :: Interpret MonadRestApi (MonadState MockData)
mockApplication = mockHttp . runRestApi
```

As you can see,
this pattern makes it easy to compose interpreters in multiple ways.
By interpreting a REST API on top of an HTTP monad,
you can swap out the HTTP interpreter to change the overall behavior.

Functors
---

Functors are a way of translating from one category to another.
The `Functor` class in Haskell represents functors from Hask to Hask.
The `InterpretFunctor` class represents functors from `Interpret` to Hask.

```haskell
class InterpretFunctor f where
  imap :: Interpret a b -> f a -> f b
```

This could be more general,
by using the Category and Functor classes from the `hask` package.
But that would be beyond the scope of this package.
Also, when `Generic1` [becomes poly-kinded](https://phabricator.haskell.org/D2168),
`InterpretFunctor` will be derivable.

A good example of an `InterpretFunctor` is the idea of a `Services` record type.

```haskell
data Services c = Services
  { runHttp :: Interpret MonadHttp c
  , runSql :: Interpret MonadSql c
  }

instance InterpretFunctor Services where
  imap f Services {..} = Services { runHttp = runHttp . f, runSql = runSql . f }

application :: c m => Services c -> m ()
application Services {..} = do
  respone <- interpret runHttp (httpGet "url")
  ...
```

This makes your application subject to the same composability as interpreters.
