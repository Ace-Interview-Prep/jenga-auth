{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Jenga.Backend.DB.BeamInstances where

{-
For creating Beam instances for returning larger tuples
-}

--for stupid instances
import Database.Beam.Query (QExprToIdentity)
import Database.Beam.Query.Internal
  ( ThreadRewritable, WithRewrittenThread, rewriteThread
  , ProjectibleWithPredicate(..)
  )
import Database.Beam.Backend.Types (BeamBackend, Exposed, Nullable)
import Database.Beam.Backend.SQL.Row (FromBackendRow(..), FromBackendRowM)
--import Database.Beam.Backend.SQL.Row as X


import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Rep, R, K1(..), U1(..), M1(..), (:*:)(..), to)
import GHC.Types (Type)
import Data.Functor.Identity (Identity)




deriving instance Generic ((,,,,,,,,) a b c d e f g h i)

-- | I copied this cuz its not exported
-- TODO: upstream this bullshit module
class GFromBackendRow be (exposed :: Type -> Type) rep where
  gFromBackendRow :: Proxy exposed -> FromBackendRowM be (rep ())
  gValuesNeeded :: Proxy be -> Proxy exposed -> Proxy rep -> Int
instance GFromBackendRow be e p => GFromBackendRow be (M1 t f e) (M1 t f p) where
  gFromBackendRow _ = M1 <$> gFromBackendRow (Proxy @e)
  gValuesNeeded be _ _ = gValuesNeeded be (Proxy @e) (Proxy @p)
instance GFromBackendRow be e U1 where
  gFromBackendRow _ = pure U1
  gValuesNeeded _ _ _ = 0
instance (GFromBackendRow be aExp a, GFromBackendRow be bExp b) => GFromBackendRow be (aExp :*: bExp) (a :*: b) where
  gFromBackendRow _ = (:*:) <$> gFromBackendRow (Proxy @aExp) <*> gFromBackendRow (Proxy @bExp)
  gValuesNeeded be _ _ = gValuesNeeded be (Proxy @aExp) (Proxy @a) + gValuesNeeded be (Proxy @bExp) (Proxy @b)
instance FromBackendRow be x => GFromBackendRow be (K1 R (Exposed x)) (K1 R x) where
  gFromBackendRow _ = K1 <$> fromBackendRow
  gValuesNeeded be _ _ = valuesNeeded be (Proxy @x)
instance FromBackendRow be (t Identity) => GFromBackendRow be (K1 R (t Exposed)) (K1 R (t Identity)) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded be _ _ = valuesNeeded be (Proxy @(t Identity))
instance FromBackendRow be (t (Nullable Identity)) => GFromBackendRow be (K1 R (t (Nullable Exposed))) (K1 R (t (Nullable Identity))) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded be _ _ = valuesNeeded be (Proxy @(t (Nullable Identity)))
-- instance BeamBackend be => FromBackendRow be () where
--   fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep ()))
--   valuesNeeded _ _ = 0


type instance QExprToIdentity (a, b, c, d, e, f, g, h, i) =
  ( QExprToIdentity a
  , QExprToIdentity b
  , QExprToIdentity c
  , QExprToIdentity d
  , QExprToIdentity e
  , QExprToIdentity f
  , QExprToIdentity g
  , QExprToIdentity h
  , QExprToIdentity i
  )

instance ( ThreadRewritable s a, ThreadRewritable s b, ThreadRewritable s c, ThreadRewritable s d
         , ThreadRewritable s e, ThreadRewritable s f, ThreadRewritable s g, ThreadRewritable s h
         , ThreadRewritable s i
         ) =>
  ThreadRewritable s (a, b, c, d, e, f, g, h, i) where
  type WithRewrittenThread s s' (a, b, c, d, e, f, g, h, i) =
    ( WithRewrittenThread s s' a, WithRewrittenThread s s' b, WithRewrittenThread s s' c, WithRewrittenThread s s' d
    , WithRewrittenThread s s' e, WithRewrittenThread s s' f, WithRewrittenThread s s' g, WithRewrittenThread s s' h
    , WithRewrittenThread s s' i
    )
  rewriteThread s' (a, b, c, d, e, f, g, h, i) =
    ( rewriteThread s' a, rewriteThread s' b, rewriteThread s' c, rewriteThread s' d
    , rewriteThread s' e, rewriteThread s' f, rewriteThread s' g, rewriteThread s' h
    , rewriteThread s' i
    )



instance ( ProjectibleWithPredicate contextPredicate be res a, ProjectibleWithPredicate contextPredicate be res b, ProjectibleWithPredicate contextPredicate be res c
         , ProjectibleWithPredicate contextPredicate be res d, ProjectibleWithPredicate contextPredicate be res e, ProjectibleWithPredicate contextPredicate be res f
         , ProjectibleWithPredicate contextPredicate be res g, ProjectibleWithPredicate contextPredicate be res h, ProjectibleWithPredicate contextPredicate be res i
         ) => ProjectibleWithPredicate contextPredicate be res (a, b, c, d, e, f, g, h, i) where
  project' context be mkE (a, b, c, d, e, f, g, h, i) =
    (,,,,,,,,)
    <$> project' context be mkE a <*> project' context be mkE b <*> project' context be mkE c
    <*> project' context be mkE d <*> project' context be mkE e <*> project' context be mkE f
    <*> project' context be mkE g <*> project' context be mkE h <*> project' context be mkE i
  projectSkeleton' context be mkM =
    (,,,,,,,,)
    <$> projectSkeleton' context be mkM
    <*> projectSkeleton' context be mkM
    <*> projectSkeleton' context be mkM
    <*> projectSkeleton' context be mkM
    <*> projectSkeleton' context be mkM
    <*> projectSkeleton' context be mkM
    <*> projectSkeleton' context be mkM
    <*> projectSkeleton' context be mkM
    <*> projectSkeleton' context be mkM

instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f
         , FromBackendRow be g, FromBackendRow be h, FromBackendRow be i ) =>
  FromBackendRow be (a, b, c, d, e, f, g, h, i) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g, Exposed h, Exposed i)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e) + valuesNeeded be (Proxy @f) + valuesNeeded be (Proxy @g) + valuesNeeded be (Proxy @h) +
                      valuesNeeded be (Proxy @i)
