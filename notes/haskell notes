
### Typeclasses

#### Functors
A functor is a "context" containing some data. It allows transformation of the data with a function
supplied via `fmap` or `<$>` while retaining the context. For example `(*2) <$> [5]` returns
`[10]`. The data is `5`, the context is the list, the transformation is `(*2)`. The context is
retained after the transformation.
```
msk|nixos 0  $ ghci
GHCi, version 8.10.3: https://www.haskell.org/ghc/  :? for help
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```
If multiple types implement the Functor typeclass, this allows us to write generic code that
transforms the contained data without concerning ourselves with the implementation of the context.
For example, we can use the same interface, `fmap`, on a list, a tree, a set, a `Maybe` etc.

#### Applicative Functors
An applicative functor is a "context" containing some data. It allows application of a function
contained in the context to data contained in another context of the same type using `<*>`. For
example `[(+3)] <*> [5]` returns `[8]`. The function `(+3)` inside the first context (the list) is
applied to the data `5` inside the second context. The result remains in the context. The key
functionality applicative functors enable is application of functions of multiple parameters to
data contained in a context. E.g. `(+) fmap [3] <*> [5]`. This is equivalent to
`((+) fmap [3]) <*> [5]`. Specifically:
1. `+` takes two arguments
2. it is initially `fmap`ped "into" the context and applied to `3`, returning `[(+3)]`
3. the expression is then `[(+3)] <*> [5]`
4. the result is `[5+3]` or `[8]`

```
msk|nixos 0  $ ghci
GHCi, version 8.10.3: https://www.haskell.org/ghc/  :? for help
Prelude> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```
If multiple types implement the Applicative Functor typeclass, this allows us to write generic code
that transforms the contained data without concerning ourselves with the implementation of the
context.  For example, we can use the same interface, `<*>`, on a list, a tree, a set etc.

#### Monads
A monad is a "context" containing some data. It allows application of a function that transforms
the data and returns new data inside a context of the same type using `>>=` (pronounced _bind_).
For example, concatenating two user inputs from `getLine`:
```haskell
getLine >>= \s -> fmap (s++) getLine
```
In this example:
1. The first `getLine` is a "context" called `IO` containing some data
2. `\s -> fmap (s++) getLine` is a function that transforms the input data and returns new data in
   a context of the same type, `IO`

The key functionality that monads enable is to allow us to combine operations that return data
inside a context. See also the better example here:
https://en.wikibooks.org/wiki/Haskell/Understanding_monads
