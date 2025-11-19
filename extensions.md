# Language extensions used in Jenga

This project relies on a number of GHC language extensions to support advanced type-level programming, generic deriving, Template Haskell, and ergonomic application code.

Each subsection links directly to the relevant part of the GHC User's Guide for that extension.

Note that anything newer than Haskell98 is technically considered a language-extension however many of these are very well supported and apart of the [GHC2024 Extension Set](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html#extension-GHC2024)

---

## Type system / polymorphism

### AllowAmbiguousTypes

> Allow type signatures that would otherwise be rejected as ambiguous, relying on use-sites (and type applications) to disambiguate.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ambiguous_types.html#extension-AllowAmbiguousTypes>

---

### ConstraintKinds

> Treat constraints as first-class types of kind `Constraint`, allowing type synonyms and abstractions over constraint sets.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/constraint_kind.html#extension-ConstraintKinds>

---

### DataKinds

> Promote data constructors to the type level, enabling type-level natural numbers, symbols, and more.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html#extension-DataKinds>

---

### PolyKinds

> Generalise kinds so that type constructors can be polymorphic in their kind parameters, not just `Type`.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/poly_kinds.html#extension-PolyKinds>

---

### KindSignatures

> Allow explicit kind annotations for type variables and type constructors.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/kind_signatures.html#extension-KindSignatures>

---

### RankNTypes

> Enable higher-rank polymorphism, e.g. `forall a. a -> f a` in argument positions.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/arbitrary_rank_polymorphism.html#extension-RankNTypes>

---

### ScopedTypeVariables

> Make explicitly quantified type variables in a signature scope over the body of a definition.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables>

---

### TypeApplications

> Allow visible type application syntax (`funcFoo @Int`) at use-sites.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/visible_type_applications.html#extension-TypeApplications>

---

### TypeFamilies

> Enable type families (type-level functions) associated with type classes or defined at the top level.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#extension-TypeFamilies>

---

### TypeOperators

> Allow symbolic names for types (e.g. `a :-> b`), and treat them as infix operators.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_operators.html#extension-TypeOperators>

---

### QuantifiedConstraints

> Allow `forall` inside constraints, e.g. `forall a. c a => d a`, expressing higher-rank relationships between instances.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/quantified_constraints.html#extension-QuantifiedConstraints>

---

### ExistentialQuantification

> Allow existentially-quantified type variables in data constructors, hiding type information behind a value.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html#extension-ExistentialQuantification>

---

## Classes, instances, and constraints

### MultiParamTypeClasses

> Allow type classes with more than one parameter, e.g. `class HasDB env conn | env -> conn`.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/multi_param_type_classes.html#extension-MultiParamTypeClasses>

---

### FunctionalDependencies

> Attach functional dependencies (`| a -> b`) to multi-parameter classes to guide instance resolution and improve type inference.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/fundeps.html#extension-FunctionalDependencies>

---

### FlexibleContexts

> Relax restrictions on the shape of contexts in type signatures (e.g. allow nested type constructors in constraints).

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/flexible_contexts.html#extension-FlexibleContexts>

---

### FlexibleInstances

> Relax restrictions on the shape of instance heads, allowing instances for more complex type expressions.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-FlexibleInstances>

---

### UndecidableInstances

> Allow instance declarations that may not satisfy the usual termination/coverage conditions, needed for some advanced type-level patterns.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-UndecidableInstances>

---

### InstanceSigs

> Allow explicit type signatures for individual methods inside instance declarations.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instance_sigs.html#extension-InstanceSigs>

---

### DefaultSignatures

> Support default method implementations whose type is more general than the class method type, commonly used with `Generic`.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/default_signatures.html#extension-DefaultSignatures>

---

### RoleAnnotations

> Allow explicit role annotations (`type role T nominal representational`) for type parameters, controlling representational equality and safe use of `coerce`.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/roles.html#extension-RoleAnnotations>

---

## Deriving-related extensions

### DeriveGeneric

> Automatically derive `Generic` / `Generic1` instances used for generic programming (e.g. JSON, DB schemas).

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html#extension-DeriveGeneric>

---

### DeriveDataTypeable

> Automatically derive `Data` (and historically `Typeable`) instances for generic traversals and reflection-like libraries.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html#extension-DeriveDataTypeable>

---

### DeriveFunctor

> Allow `deriving Functor` for parameterised types with appropriate structure.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html#extension-DeriveFunctor>

---

### DeriveFoldable

> Allow `deriving Foldable` for traversable data structures.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html#extension-DeriveFoldable>

---

### DeriveTraversable

> Allow `deriving Traversable`, which also implies appropriate `Functor` and `Foldable` behaviour.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html#extension-DeriveTraversable>

---

### DeriveLift

> Automatically derive `Lift` instances to embed values in Template Haskell splices.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html#extension-DeriveLift>

---

### GeneralizedNewtypeDeriving

> Automatically derive instances for a `newtype` based on its underlying representation using `coerce`.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html#extension-GeneralizedNewtypeDeriving>

---

### StandaloneDeriving

> Allow `deriving` clauses as separate top-level declarations, not just attached to the original data type.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/standalone_deriving.html#extension-StandaloneDeriving>

---

### DerivingStrategies

> Enable `deriving stock`, `deriving newtype`, `deriving anyclass`, and `deriving via` strategy keywords.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_strategies.html#extension-DerivingStrategies>

---

## Data types and patterns

### GADTs

> Generalised algebraic data types: link constructor result types and parameters in richer ways and enable more precise pattern matching.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html#extension-GADTs>

---

### EmptyDataDecls

> Allow data type declarations with no constructors.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/nullary_types.html#extension-EmptyDataDecls>

---

### EmptyCase

> Allow `case` expressions (and `\case`) with zero alternatives, often used to express impossibility for empty types.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/empty_case.html#extension-EmptyCase>

---

### TypeOperators

> (Also listed above under type system.) Allow type constructors like `(a :*: b)` using symbolic operators.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_operators.html#extension-TypeOperators>

---

## Syntax and layout

### BangPatterns

> Enable `!pat` syntax for strict pattern bindings and strict `let`/`where` bindings.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/strict.html#extension-BangPatterns>

---

### BlockArguments

> Allow `do`, `case`, `if`, `lambda`, `mdo`, and `\case` blocks to be passed as function arguments without extra parentheses or `$`.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/block_arguments.html#extension-BlockArguments>

---

### LambdaCase

> Provide `\case` syntax for anonymous functions that immediately pattern match on their argument.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lambda_case.html#extension-LambdaCase>

---

### RecursiveDo

> Add `mdo` / `rec` notation for monadic recursive bindings (used heavily in FRP-style code like `reflex`).

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html#extension-RecursiveDo>

---

### TupleSections

> Enable tuple sections such as `(, x)` or `(a, , c)` as partially-applied tuple constructors.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/tuple_sections.html#extension-TupleSections>

---

### CPP

> Run the C preprocessor (`cpp`) over Haskell source files, enabling `#if`, `#ifdef`, etc. for conditional compilation.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/cpp.html#extension-CPP>

---

### NoMonomorphismRestriction

> Disable the Haskell 2010 monomorphism restriction so bindings without signatures remain polymorphic in more cases.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/monomorphism.html#extension-MonomorphismRestriction>

(Use the `NoMonomorphismRestriction` flag form of this extension.)

---

## Literals and overloading

### OverloadedStrings

> Make string literals polymorphic via the `IsString` typeclass (so `"foo"` can be `Text`, `ByteString`, etc.).

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html#extension-OverloadedStrings>

---

## Metaprogramming

### TemplateHaskell

> Enable Template Haskell quotations and splices (`[| .. |]`, `$(..)`) for compile-time metaprogramming and code generation.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/template_haskell.html#extension-TemplateHaskell>

---

### QuasiQuotes

> Enable quasiquotation syntax (`[quoter| ... |]`) to embed and parse domain-specific languages such as HTML or Markdown.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/quasi_quotes.html#extension-QuasiQuotes>

---

## Miscellaneous

### RoleAnnotations

> (Also listed above under classes/instances.) Allow explicit role annotations for type parameters.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/roles.html#extension-RoleAnnotations>

---

### GeneralizedNewtypeDeriving

> (Also listed above under deriving.) Derive newtype instances based on representation equality.

Docs: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html#extension-GeneralizedNewtypeDeriving>
