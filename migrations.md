# Migrations in Jenga

A migration will only occur when we change our `Db` type in ./common/src/Common/Schema.hs or change a database setting (like explained in [beam-automigrate](https://github.com/obsidiansystems/beam-automigrate?tab=readme-ov-file#overriding-the-defaults-of-an-annotateddatabasesettings). Even if we manually perform a query to alter our database, it will more likely than not be overridden by a new migration by beam-automigrate. This is because our database **must** match the description of the `Db` type.

## Safe Migrations

Safe migrations, ones which wont destroy any data are automatically run, however it is possible to override this like how is shown in [Unsafe Simple Migrations](#unsafe-simple-migrations) but with `markUnsafe` over `markSafe`. This will then mean a failure would happen at startup if a migration fitting that description was ever attempted. Personally I have seen no use case for this in the beam-automigrate context.

## Unsafe Simple Migrations

In ./backend/src/Backend/DB/Migrations.hs we have this code

```haskell
-- | NOTE: we dont need to sort at all cuz the function from
-- | beam-automigrate does it for us after the hook
-- | however, out of paranoia we sort anyways, in case they cease to do so
whitelistedUnsafeMigrations :: [WithPriority Edit] -> [WithPriority Edit]
whitelistedUnsafeMigrations edits =
  let (unsafe, safe) = splitEditsOnSafety edits
  in  sortEdits $ safe <> fmap reviewSteps unsafe
  where
    reviewSteps :: WithPriority Edit -> WithPriority Edit
    reviewSteps (WithPriority (edit, priority)) = WithPriority ( fromUnexpectedUnsafe edit, priority )

    fromUnexpectedUnsafe :: Edit -> Edit -- Maybe [Edit]
    fromUnexpectedUnsafe this = this
```

This code stub is given as a prescription for how to manage safe migrations in beam-automigrate.

Here is an example whitelist from our own repo at the time of writing this doc

```haskell
    fromUnexpectedUnsafe :: Edit -> Edit -- Maybe [Edit]
    fromUnexpectedUnsafe this = case (_editAction this) of
      EditAction_Manual _mEdit -> this
      EditAction_Automatic auto -> case auto of
        TableRemoved tblName -> case tblName of
          "groupChat2" -> markSafe this
          "mockNags" -> markSafe this
          "task" -> markSafe this
          _ -> this
        SequenceRemoved seqName_ -> case seqName seqName_ of
          "task___taskId___seq" -> markSafe this
          "task_taskId_seq" -> markSafe this
          "groupChat2___id___seq" -> markSafe this

          _ -> this
        TableConstraintRemoved _tblName _tblConstraint _tblConstraintRemovedType
          | tableName _tblName == "task"
            && unConsraintName _tblConstraint == "task_pkey"
            -> markSafe this
          | otherwise -> this
        ColumnRemoved _tblName _colName -> this
        ColumnTypeChanged tblName colName _oldType newType
          | tableName tblName == "task"
            && columnName colName == "created_at2"
            && newType == SqlStdType AST.DataTypeInteger
            -> markSafe this
          | tableName tblName == "reporting"
            && columnName colName == "payload"
            -> markSafe this
          | otherwise -> this
        ColumnDefaultChanged _tblName _colName _mDefConstr
          | tableName _tblName == "task"
            && columnName _colName == "taskId"
            -> markSafe this
          | otherwise -> this

        EnumTypeRemoved _enumerationName -> this
        _ -> this
```

As the code shows, we are pattern matching on particular edits like removing a table named groupChat2. Removing a table is just one case of AutomaticEditAction. For a full list, we can edit safety on, see below

```haskell

data AutomaticEditAction
  = TableAdded TableName Table
  | TableRemoved TableName


  | PrimaryKeyAdded TableName PrimaryKeyConstraint UniqueConstraintOptions
  | UniqueConstraintAdded TableName Unique UniqueConstraintOptions
  | ForeignKeyAdded TableName ForeignKey ForeignKeyConstraintOptions

  | TableConstraintRemoved TableName ConstraintName TableConstraintRemovedType
  | RenameConstraint TableName
    ConstraintName {- old name -}
    ConstraintName {- new name -}

  | ColumnAdded TableName ColumnName Column
  | ColumnRemoved TableName ColumnName
  | ColumnTypeChanged TableName ColumnName ColumnType {- old type -} ColumnType {- new type -}
  | ColumnNullableChanged TableName ColumnName NullableConstraint {- is nullable -}
  | ColumnDefaultChanged TableName ColumnName (Maybe DefaultConstraint)
  | EnumTypeAdded EnumerationName Enumeration
  | EnumTypeRemoved EnumerationName
  | EnumTypeValueAdded EnumerationName Text {- added value -} InsertionOrder Text {- insertion point -}
  | SequenceAdded SequenceName (Maybe Sequence)
  | SequenceRemoved SequenceName
  | SequenceRenamed
    SequenceName {- old name -}
    SequenceName {- new name -}
  | SequenceSetOwner SequenceName (Maybe Sequence)
  deriving (Show, Eq)
```

# Unsafe Complex Migrations

There are only a few cases which cannot be easily handled by beam-automigrate, however this would all be more frustrating than not if we did not have the ability to run arbitrary queries. While we are able to run any script to mutate database state immediately before we startup the server, it is best to apply the changes through beam-automigrate, which embeds all actions in a transaction so that if at any point there is a failure we can rollback the database state.

In ./backend/src/Backend/DB/Migrations.hs there is a code stub like this:

```haskell
runMigrations :: Connection -> IO ()
runMigrations conn = do
  tryRunMigrationsWithEditUpdateAndHooks
    preHook
    postHook
    (filterMigrations . whitelistedUnsafeMigrations)
    (fromAnnotatedDbSettings checkedDb (Proxy.Proxy @'[]))
    conn
```

as well as the associated preHook and postHook functions

```haskell
preHook :: Pg ()
preHook = pure ()

postHook :: () -> Pg ()
postHook = \() -> pure ()
```

It is not obvious here, but the value returned by preHook is passed to postHook, via the `tryRunMigrationsWithEditUpdateAndHooks` function. As Pg is the generic monad for interacting/running queries with a postgres database, we can perform arbitrary logic before and after beam-automigrate performs it's `Edit`s. For example, if we are removing a table from `Db` but want that table's data to be added to other tables, we could query a Table named "ToDelete" to get all rows in preHook, have beam-automigrate delete "ToDelete", then in `postHook`, allocate that data we have successfully queried into the respective tables.

# Performing Migrations on a Production Database

Migrations will happen on startup of the backend server. This means when a new git commit hash is deployed to production, if it contains changes the schema then this migration will then attempt to run. For that reason you should ensure that no unsafe simple changes, which have not been marked as safe are expected to happen.

If a migration fails, such as for an unexpected unsafe migration, then the server will fail to start. This is to avoid any potential for unexpected failures as your server handles requests.

To have absolute certainty that a given deployment will succeed, a good idea is to run the build of the commit hash you intend to deploy against a copy of the database or at least it's schema before deploying through `ob deploy push`.

For more context on Obelisk deployments see [here](https://github.com/obsidiansystems/obelisk?tab=readme-ov-file#deploying).
