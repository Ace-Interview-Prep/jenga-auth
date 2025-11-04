module Jenga.Frontend.Consumable where

import Reflex.Dom.Core
import Control.Monad.Fix

type Consumable a = Maybe [a]


-- | This is part of an Event framework, consider if you used holdDyn for an event
-- | That starts as empty because it hasnt been received yet (ie. Nothing)
-- | and will be consumed or in some way the Dynamic will become empty again because
-- | of usage. So here Just [] or justNull means it has been used up.
-- | Typically that means that we should gate the action of asking for more information like from the
-- | server
justNull :: Consumable a -> Bool
justNull = \case
  Just [] -> True
  _ -> False

depleted :: Consumable a -> Bool
depleted = justNull

holdConsumable :: (Reflex t, MonadHold t m) => Event t [a] -> m (Dynamic t (Consumable a))
holdConsumable e = holdDyn Nothing $ Just <$> e

foldConsumable :: (Reflex t, MonadHold t m, MonadFix m) => Event t [a] -> m (Dynamic t (Consumable a))
foldConsumable e = foldDyn f Nothing e
  where
    f new_ = \case
      Nothing -> Just new_
      Just old -> Just $ old <> new_

dropDyn :: (MonadHold t m, Reflex t, MonadFix m) => Event t () -> [a] -> m (Dynamic t [a])
dropDyn e xs = foldDyn (\() xs' -> drop 1 xs') xs e

consumeWhen :: (MonadHold t m, Reflex t, MonadFix m) => Event t () -> [a] -> m (Dynamic t (Maybe a))
consumeWhen e xs = (fmap . fmap) f $ dropDyn e xs
  where
    f = \case
      [] -> Nothing
      (x_:_) -> Just x_
