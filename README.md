
<!-- ![jenga](./jenga.png) -->
<!-- <img src="./jenga.png" width="48"> -->
# Jenga Stack

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)

<p align="center"><img src="jenga.png" width="50%" alt="Jenga Logo"></p>

Jenga is

- A full-stack template (frontend, backend, database)
- multi-platform (Android, iOS, Web)
- 100% searchable through a local hoogle.

It's also my dog's name.

# To Learn Jenga

See "Learn Jenga" under [Tutorials](#tutorials)

A good few tips for learning Jenga are to let Hoogle and compiler errors be your guiding light. While the compiler errors may feel overwhelming to read at first, they are the best tool that Haskell has. When you are comfortable with an area of programming it feels like working through a TODO list of changes you need to make for a new feature to function perfectly well with respect to the rest of the existing code.

# README sections

- [Documentation](#documentation)
  - [tutorials](#tutorials)
  - [hoogle](#hoogle)
  - [Backend](#backend)
	- [Production-Level Authentication](#generalized-production-level-auth)
		- [Stripe Subscriptions](#subscription-handling-through-stripe)
		- [Role-Based Access Control](#role-based-access-control)
		- [APIs](#apis)
		- [Generic Auth Request Handlers](#generic-auth-handlers)
	- [Production Level Error Reporting](#production-level-error-reporting)
	- [Websockets for Real-Time Updates](#views-notify-websockets-for-realtime-updates)
	- [Task Workers](#workers)
	- [Jenga Config](#config-utils)
	- [Generalized Database Access](#generalized-database-access)
		- [TypeApplications](#type-applications)
	- [ReaderT Pattern](#readert-pattern)
	- [Database as a Type](#database-as-a-type)
		- [Table as a type](#table-as-a-type)
		- [Migrations](#migrations)
  - [Server Deployment](#server-deployment)
  - [Thunks](#thunks)
  - [Associated Libraries](#associated-library-list)

# Documentation
## Tutorials

See [tutorials.md](./tutorials.md)

## hoogle

Hoogle, which we show how to setup in [Run Hoogle section](#run-hoogle-server) is an amazing tool that shows how self-documenting Haskell code is. We can use Hoogle to browse documentation as well as go see exactly how a type or function behaves.

# Backend

This is an amazing production ready, out of the box template.

## Generalized Production Level Auth

Across the entire stack, you can use jenga-auth to immediately have role-based authentication, subscriptions, the necessary handlers for email-based authentication as well as OAuth.

We also provide frontend FRP networks that only require you to build a dom which provide the user a way to interact with your login page.

For example with your login template if you can return:

```haskell
-- from Jenga.Frontend.Auth.Login
data LoginData t m = LoginData
  { _login_username :: InputEl t m
  , _login_password :: InputEl t m
  , _login_submit :: Event t ()
  , _login_forgotPassword :: Event t ()
  }
```

and you have called this function in Backend.hs:

```haskell
loginHandler
  :: forall db cfg m.
     ( ...constraints...
     )
  => (T.Text, Password)
  -> ReaderT cfg m (Either (BackendError LoginError) (Signed (Id Account), UserType))
```

Then you have written enough to have a login feature. See [jenga-auth](https://github.com/Ace-Interview-Prep/jenga-auth) for more details on implementation.

Same deal exists for:

- resetPassword
- requestResetPassword
- OAuth
- stripe-based subscriptions
- Admin-based
  - Add sub-user
  - adminSignup
  - etc.

To see how to provide functionality behind this auth framework and only for appropriately authenticated users, see [Generic Auth Handlers](#generic-auth-handlers)

### Subscription Handling Through Stripe

For apps that require payment we also provide helper functions for this, to manage subscriptions. This of course requires an API key to be managed by your jenga app under `configs/backend/` directory

See [tutorials](#tutorials) for how to setup stripe.

### Role Based Access Control

As long as you use certain database tables defined by Jenga and Rhyolite, you are able to differentiate users by `Self` (controls no other users) and `Admin` (controls other users, ie ones they have added). We then make it easy to restrict functionality based on user type, through JSON+Auth Handlers.

See [tutorials](#tutorials) for how to setup role-based access control.

## APIs

There are 2 separate ways to handle an API request in Jenga

1. HTTP-Based through [snap](http://snapframework.com/)
2. Authenticated JSON over Websockets

You can look at backend/src/Backend.hs, using `serve` for an example of 1. and backend/src/Websockets/Request.hs requestHandler for an example of 2. which uses Rhyolite.Api. Both have their own way of ensuring correct authentication.

### Generic Auth Handlers

We demonstrate an example type signature for one of Jenga's generic Auth handlers. In plain english this means if our environment has access to a signing key (`CS.Key`), the name of your App's auth cookie, your database connection, and your admin email in addition to having a database with a user-type table, log table, and email task table THEN we can use this higher-order function.

`withDependentPrivateJSONRequestResponse` provides a way for us to use the UserType and Id of the user that jenga-auth has declared to be correct based on the signing key you provided through config. So for example we could implement /getMyUserName as an API to either get the organizational name for an admin user or get the username for a normal user.

```haskell
withDependentPrivateJSONRequestResponse
  :: forall db be a b m e cfg n
  . ( FromJSON a
    , ToJSON b
    , ToJSON e
    , Show b
    , Show e
    , SpecificError (BackendError e)
    , MonadSnap m
    , Database Postgres db
    , HasConfig cfg CS.Key
    , HasConfig cfg AuthCookieName
    , HasConfig cfg (Pool Connection)
    , HasConfig cfg AdminEmail
    , HasJengaTable Postgres db UserTypeTable
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => (UserType -> Id Account -> a -> ReaderT cfg m (Either (BackendError e) b))
  -> ReaderT cfg m ()
withDependentPrivateJSONRequestResponse withF = do
```

While that may look overwhelming, it simply allows us a way to host a function such as `myHandler`

```haskell
data SomeError = ...
data GoodResponse = ...
data MyConfig = ...

myHandler :: Request -> ReaderT MyConfig IO (Either (BackendError SomeError) GoodResponse)
```

If this still feels overwhelming, it is likely the "ReaderT MyConfig IO" piece. You likely need to learn about the concept of Monads, and get comfortable with using them. Here we use the concept to establish a common context for our application, which is a context that has 'pure' access to a config, which we create at the startup of our application. Monads and purity are two important topics that are well discussed in functional programming but for the sake of a simple demonstration, you should look at [Config Utils](#config-utils) as a use case of monads.

## Production Level Error Reporting

Jenga is essentially the by-product of stripping out Ace's core architecture. As a startup, we need to be aware of any issues fast, so we use the higher-order function `withErrorReporting` from Jenga.Backend.Utils.ErrorHandling that ensures we are via email of any major issues immediately and receive a log of smaller user-errors which may indicate UX issues through an aggregated, less-urgent report.

Usage is incredibly straightforward if you are using functions from Jenga. For instance here's a full implementation that Jenga's loginHandler function uses:

```haskell
data LoginError
  = UnrecognizedEmail T.Text
  | IncorrectPassword
  | NoUserTypeFound
  | ExpiredSubscription T.Text
  | ExpiredFreeTrial T.Text
  deriving (Eq,Show,Generic)
instance FromJSON LoginError
instance ToJSON LoginError
instance ShowUser LoginError where
  showUser (UnrecognizedEmail _) = "An account with this email doesn't exist"
  showUser IncorrectPassword =  "Incorrect password, please try again"
  showUser NoUserTypeFound =  "No UserType found"
  showUser (ExpiredSubscription subscribeURL) =  "Your subscription is no longer active. Please visit " <> subscribeURL
  showUser (ExpiredFreeTrial userEmail) = "The free trial for " <> userEmail <> " is expired"

instance SpecificError (BackendError LoginError)
```

We choose logical cases for errors that can happen, give it JSON instances, define how it should look to the user and give an instance for SpecificError as a subset of the more generic BackendError. We now have what is needed to have perfect error reporting both to the user and ourselves.

## Views + Notify: Websockets for Real-Time Updates

A typical websocket dataflow looks something like

```bash
 first req ==>  /listen        ==> first response  ==> store new value ==> do something else
                                     ^^^|^^^
NewChange ==> DbNotification  ==> nth response
```

In Rhyolite, which we use for this websocket functionality, this maps to

```bash
 first req ==>  /listen        ==> first response  ==> holdDyn ==> Dynamic t a ==> Perform (SomeEvent `or` DomAction)
                                     ^^^|^^^
NewChange ==> DbNotification  ==> nth response
```

Additionally:

- first req ==> /listen -> Set up in Common.Route using Obelisk.Route
- NewChange ==> DbNotification -> Using Rhyolite, we provide the backend a way to respond to specific DB notifications and feed websocket pipelines
- the Reflex library easily fits this dataflow to it's [event system](https://docs.reflex-frp.org/en/latest/reflex_docs.html#frp-basics) on the frontend application.

Through Rhyolite we can create anything from one subscription to a tree of typed subscriptions using the [Vessel package](https://github.com/obsidiansystems/vessel). On each leaf of this API, a Rhyolite 'View' describes what a user sees on page load, and a Rhyolite 'Notify' changes that value as time goes on.

There is a [vessel tutorial here](https://github.com/obsidiansystems/vessel/blob/develop/tutorial/Tutorial.md) which assumes proficiency with haskell, reflex-dom and advanced haskell concepts like GADTs and monad transformers.

## Workers

Easily spawn tasks on an interval. see Backend.Workers.\* for examples

## Config Utils

We use ReaderT to provide us a 'pure' Config. This builds on the tooling that Obelisk gives us to work with the configs directory. Essentially for Jenga to use Configs in a pure manner we need to handle all 'impure' actions at the startup of the application, using Obelisk.Configs.

To demonstrate what is meant by purity take these two cases

```haskell
-- get the configs directory as a Map, with the FilePaths found as keys
cfgs <- Obelisk.Config.getConfigs
pure $ MkConfig $ cfgs !? "common/route"
```

This is an impure step, because it reads data (our route) from the server's filesystem (outside the boundary of our application) and thus can fail for any uninteresting reason. For example if a new developer on the team changes the filename, the same program will fail *after that point in time* but not because our code changed. This helps to explain the formal definition of purity, which is that a function is pure, if for a given list of input arguments, it will provide the same outputs. For example, 1+1 is 2 regardless of what the time of day (or year, or millenium, or planet, or multiverse...) whereas `currentTimeAsInt + 1` will alwaaaaays provide a different output, so it is impure.

So the central idea, is if we can make our usage of `Config` pure then we can simplify a ton of our thinking, design, and code. For instance, we can create the entire jenga-auth library on this basis, that we knew some way or another your environment will provide Jenga functions exactly what is needed. We could of course just pass arguments like any normal haskell function however `HasConfig` allows us to focus on what business logic the handler is actually doing.

This also means we can think of ReaderT as being a 'pure' Monad, and the IO Monad which we use to get the actual config directory as an 'impure monad', at least for getting access to config. We can still perform whatever impure actions with this purely gatherered config.

Putting this concept into use, we can know that this action will never fail

```haskell
func :: ...constraints... => ReaderT Config m Link
func = do
  (enc :: FullRouteEncoder be fe) <- asksM
  BaseURL baseUrl <- asksM
  pure . Link $ (T.pack $ show baseUrl) <> renderBackendRoute enc route
```

This pseudo-looking `func` is actually the function `renderFullRouteBE` from Jenga.Common.HasJengaConfig and it guarantees that we are properly building a link, such as a one-time-password (OTP) link.

You might notice that we use `asksM` twice but it returns two different types. This is ultimately possible because the compiler knows what type that value *must* be. For example, renderBackendRoute is a function we call here which expects a `FullRouteEncoder` and a `URI`, so even if we didn't explicitly label them, the compiler would know how to handle this code. `asksM` provides a simple way to use the environment and even build Jenga-based libraries that can be used by the community to perform generic functions like authentication or emailing.

In practice, our generalizing of config operates through a newtype like this:

```haskell
newtype BaseURL = BaseURL {..}
```

This is so that consumers of this development API can write trivial instances to start with a library like jenga-auth by writing:

```haskell
data MyConfig = MyConfig
  { _myUrlField :: BaseURL
  , ...
  }
instance HasConfig MyConfig BaseURL where
  fromCfg = _myUrlField
```

and if the same idea is repeated for how to get the Route-encoder from our config, then we can use `renderFullRouteBE`, as well as any functions which use `renderFullRouteBE`.

In a typical Jenga app, we have a FrontendConfig and a BackendConfig since certain information should only be available to the backend (eg. API Keys to Claude)

## Generalized Database Access

The backend also makes use of our Db type defined in common/src/Common/Schema.hs, in a similar manner

```haskell
data Db = Db
  { _db_accounts :: f (TableEntity Account)
  ...
  }

-- from backend/src/Backend/Config.hs
instance HasJengaTable Postgres Db Account where
  tableRef = _db_accounts db
```

### TypeApplications

Some library functions meant for the Jenga framework are quite codebase-agnostic. For example jenga-auth is meant to work for any database type that has a couple properties, same with other core types we use from Obelisk and Rhyolite like routes. So we'll see cases like in backend/src/Backend.hs:

```haskell
Auth.Login.loginHandler @Db email_pass
```

We are instructing the compiler that while loginHandler can work with any database that fits the constraints, we are specifically using our database defined in module Common.Schema. This gives us flexibility to work with and swap out multiple databases where applicable.

In summary, we have discussed how at startup of our server, we can take from configs/ directory which may fail and turn that into a purely configured environment which can be used to safely perform environment-specific actions of any form.

## ReaderT Pattern

Note that this section is not meant to be strictly important to starting with Jenga, but might be interesting in terms of design.

ReaderT pattern originates from the concept of Monad transformers but has also gained recent attention in the domain of Effect Systems. What's funny to admit is that prior to learning about effect systems I thought ReaderT was useless. I now think that Reader/ReaderT is the best example of what I mean by a "Monad", in that I think of it as being a context we can assume we are operating out of; performing the function `ask` means "assuming we have called `runReaderT (myReaderT_Action) cfg` we can guaranteed get the value of `cfg`". This is at least the motivation for using Monads over ways we could write pure functions. This does not really do justice to explain the original Monad called `IO` which is undoubtedly the biggest challenge that monads solved, but it helps me remind myself where I would want to use Reader over just passing the config as an argument through a number of functions.

Seeing the applications of Reader/ReaderT as something recommended by those who advocate for Simple Haskell, who advocate for Effect Systems (eff, bluefin), and those who advocate for monad stacks (reflex-dom) via the mtl approach, it seems reasonable to say ReaderT is one of the most useful Monads, despite the fact that it is truly unnecessary. Unnecessary unless we consider how important it is to write good code and good libraries.

One point of research I intend on is to compare the way I've approached Jenga with Effect Systems as I'm not sure what will come of it but perhaps it will lead to Jenga being a full-fledged effect system or just stealing ideas from the domain, or realizing that structurally they are one in the same.

## Database as a Type

jenga uses [Beam](https://haskell-beam.github.io/beam/), [postgres/beam-postgres](https://haskell-beam.github.io/beam/user-guide/backends/beam-postgres/) and [beam-automigrate](https://github.com/obsidiansystems/beam-automigrate/tree/cg/pre-post-hooks) to provide

- A database schema defined via Haskell types
- Type-safe query generation
- Safe, easy to deploy migrations.

### Table as a Type

An example table: Rhyolite.Account.Account

```haskell
-- | The Account table defines user identities and how to authenticate them
data Account f = Account
  { _account_id :: Columnar f (SqlSerial Int64)
  , _account_email :: Columnar f Text
  , _account_password :: Columnar f (Maybe ByteString)
  , _account_passwordResetNonce :: Columnar f (Maybe UTCTime)
  } deriving (Generic)

instance Beamable Account

instance Table Account where
  newtype PrimaryKey Account f = AccountId
    { _accountId_id :: Columnar f (SqlSerial Int64)
    }
    deriving (Generic)
  primaryKey = AccountId . _account_id
```

### Migrations

[beam-automigrate](https://github.com/obsidiansystems/beam-automigrate/tree/cg/pre-post-hooks) is an amazing library that automatically handles trivial migrations, allows us a checkpoint for allowing or stopping unsafe migrations, and provides a way for arbitrary migration steps to be added in. It does all this while ensuring that the database type representing our Schema (ie Db) at the end of the migration is consistent with our backend request handlers.

A simple example to try is adding and removing a duplicate of `_db_accounts`

```haskell
data Db = Db
  { _db_accounts :: TableEntity Account
  , _db_accounts2 :: TableEntity Account
  }
```

You'll see the migrations steps that it will attempt.

```bash
[beam-migrate] Pre-migration
Database migration required, attempting...
        CREATE TABLE accounts2 (email VARCHAR NOT NULL , id  BIGSERIAL  NOT NULL , password BYTEA , "passwordResetNonce" TIMESTAMP WITH TIME ZONE );

        ALTER TABLE accounts2  ADD  PRIMARY KEY (id);


[beam-migrate] Auto-migration
[beam-migrate] Post-migration
[beam-migrate] Finished

# Then we remove it
[beam-migrate] Pre-migration
Database migration required, attempting...
        ALTER TABLE accounts2 ALTER COLUMN id DROP DEFAULT ;

        ALTER TABLE accounts2 DROP CONSTRAINT accounts2_pkey;

<UNSAFE>DROP SEQUENCE accounts2_id_seq;

<UNSAFE>DROP TABLE accounts2;


[beam-migrate] Auto-migration
<interactive>: Database migration error: UnsafeEditsDetected [EditAction_Automatic (ColumnDefaultChanged (TableName {tableName = "accounts2"}) (ColumnName {columnName = "id"}) Nothing),EditAction_Automatic (TableConstraintRemoved (TableName {tableName = "accounts2"}) (ConstraintName {unConsraintName = "accounts2_pkey"}) TableConstraintRemovedType_PrimaryKey),EditAction_Automatic (SequenceRemoved (SequenceName {seqName = "accounts2_id_seq"})),EditAction_Automatic (TableRemoved (TableName {tableName = "accounts2"}))]
CallStack (from HasCallStack):
  error, called at src/Database/Beam/AutoMigrate.hs:869:7 in beam-automigrate-0.1.6.0-Cn9hUHLQhEPHRsw6YKh5EW:Database.Beam.AutoMigrate
```

We see it automatically created the Table, then it failed to remove it. We can explicitly 'OK' this Action and it will function as one should expect.

See [migrations.md](./migrations.md) for more

# Server Deployment

Deployments are ridiculously easy.

See [Obelisk - Deploying](https://github.com/obsidiansystems/obelisk?tab=readme-ov-file#deploying) for setup

Then as explained, enter in the deployment directory and run:
```bash
ob deploy push
```

# Thunks

We make heavy use of [Nix-thunk](https://github.com/obsidiansystems/nix-thunk) which comes packaged with Obelisk.

Nix-thunk is a simple ergonomic wrapper around the `fetchFromGitHub` function that nixpkgs provides. It's core purpose here is to make it irrelevant whether a dependency we use is unpacked or not. This might sound funny if you are not familiar with [nix](https://nixos.org/). It can be viewed here as a package manager that allows for full control over how we build dependencies, and because of this it is incredibly common to build directly from source. This flexibility means it is incredibly easy to use libraries from the Haskell ecosystem, even our own forks of such library.

For example, let's say you are interested in changing code in the `lamarckian` package that I have developed (./thunks/lamarckian) it is trivial to do. Currently it is packed and will look like

```bash
ls thunks/lamarckian
-- shows: default.nix  github.json  thunk.nix
```

The github.json provides a link to the git repo, branch, and commit hash we are using.

First unpack:
```bash
cd jenga/
ob thunk unpack thunks/lamarckian
```

Make any change you desire to the code (as long as it compiles of course). Since this is just a git repo, maybe we fork our own version, maybe we change the branch, etc etc insert some arbitrary git changes.

While the thunk is unpacked, we can still easily run our Jenga project like we are depending on some stable package from the ecosystem.

When you're done, tested and satisfied with your change, just commit and push all changes to the forked remote. Then if you want, you can re-pack it.

```bash
ob thunk pack thunks/lamarckian
```

If you are adding another package as a thunk see [nix.md](./nix.md) for how to do so with nix.

# Associated Library List

One of the core goals of Jenga is to make the Haskell ecosystem ridiculously easy to get started with, but also provide a means for a mature codebase to make changes easily, regardless of where the change is needed. If you are looking to dig in and make changes, note the libraries below:

## Maintained by Jenga

- [jenga-auth](https://github.com/Ace-Interview-Prep/jenga-auth)
- [lamarckian](https://github.com/augyg/lamarckian)
- [templates](https://github.com/Ace-Interview-Prep/templates)
- [ClasshSS](https://github.com/augyg/ClasshSS)
- [reflex-classh](https://github.com/augyg/reflex-classh)
- [scrappy-template](https://github.com/Ace-Interview-Prep/scrappy-template)
- [scrappy-core](https://github.com/Ace-Interview-Prep/scrappy-core)
- [IStr](https://github.com/augyg/IStr)

## Documented and used by Jenga, but not maintained by us

### Sub-frameworks

- [Obelisk Framework](https://github.com/obsidiansystems/obelisk)
- [obelisk-oauth](https://github.com/obsidiansystems/obelisk-oauth)
- [rhyolite (package set)](https://github.com/obsidiansystems/rhyolite)
- [snap](https://hackage.haskell.org/package/snap)
- [stripe-core](https://hackage.haskell.org/package/stripe-core)

### Frontend
- [jsaddle](https://github.com/ghcjs/jsaddle)
- [reflex](https://github.com/reflex-frp/reflex)
- [reflex-dom-core](https://github.com/reflex-frp/reflex-dom)

### Static Site
- [mmark](https://github.com/mmark-md/mmark)
- [mmark-ext](https://github.com/mmark-md/mmark-ext)
- [ghc-syntax-highlighter](https://hackage.haskell.org/package/ghc-syntax-highlighter)
- [skylighting](https://hackage.haskell.org/package/skylighting)

### Database
- [vessel](https://github.com/obsidiansystems/vessel)
- [beam-automigrate](https://github.com/obsidiansystems/beam-automigrate)
- [beam](https://haskell-beam.github.io/beam/)
- [android-activity](https://github.com/obsidiansystems/android-activity)
- [gargoyle-postgresql](https://github.com/obsidiansystems/gargoyle)
- [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)

### See also

- [llm-with-context](https://github.com/augyg/llm-with-context)
  - Typified conversations with LLMs

## Tricks and Extensions used

See [extensions.md](./extensions.md)

# Contributing

We welcome it :) no formal rules are yet established, just make a PR and I'll review it.
