module Jenga.Frontend.FRPExtras where

import Reflex.Dom.Core


tagDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
tagDyn d e = tag (current d) e

fanMaybe :: Reflex t => Event t (Maybe a) -> (Event t (), Event t a)
fanMaybe evMaybe = fanEither $ ffor evMaybe $ \case
  Nothing -> Left ()
  Just a -> Right a

fanBool :: Reflex t => Event t Bool -> (Event t (), Event t ())
fanBool e = (() <$ ffilter not e, () <$ ffilter id e)
