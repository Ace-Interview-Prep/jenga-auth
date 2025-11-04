{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jenga.Frontend.Promise where

import Language.Javascript.JSaddle
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Lens ((^.))

newtype Promise =
  UnsafeToPromise JSVal
  deriving (MakeObject)

unsafeToPromise :: JSVal -> Promise
unsafeToPromise = UnsafeToPromise

handlePromise :: Promise -> (JSVal -> JSM b) -> (JSVal -> JSM a) -> JSM (Either a b)
handlePromise (UnsafeToPromise val_) thenHandler catchHandler = do
  mvar <- liftIO $ newEmptyMVar
  nextVal <- val_ ^. js1 ("then") (fun $ \_ _ v -> thenHandler (head v) >>= liftIO . putMVar mvar . Right)
  _ <- nextVal ^. js1 ("catch") (fun $ \_ _ err -> catchHandler (head err) >>= liftIO . putMVar mvar . Left)
  liftIO $ takeMVar mvar

promiseMaybe :: Promise -> (JSVal -> JSM a) -> JSM (Maybe a)
promiseMaybe promise thenHandler = do
  eitherToMaybe <$> handlePromise promise thenHandler (\_ -> pure ())
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe (Right x_) = Just x_
    eitherToMaybe _ = Nothing
